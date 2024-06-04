#########################################.
### For publication - total referrals ###
#########################################.

# Author: Bex Madden
# Date: 2024-05-28

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


# set DS choice

#dataset_choice <- "PT"

####  Get Shorewise data #####

df_shore <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(ref_month = floor_date(ref_rec_date, unit = "month"),
         ref_quarter = ceiling_date(ref_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) 

#dates
most_recent_month_in_data <- get_lastest_month_end(df_shore)

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

df_shore_ref <- df_shore |>
  filter(ref_month %in% date_range) |>
  select(all_of(data_keys), ref_rec_date, ref_month, ref_quarter_ending) |> 
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending)) |>
  filter(!is.na(ref_rec_date)) 


# make quarterly referral counts 

shore_quart_hb <- df_shore_ref |>
  distinct() |> 
  group_by(ref_quarter_ending, hb_name, dataset_type) |> 
  summarise(referrals = n()) 

shore_quart_sco <- df_shore_ref |>
  distinct() |> 
  group_by(ref_quarter_ending, dataset_type) |> 
  summarise(referrals = n()) |>
  mutate(hb_name = "NHS Scotland")

shore_quart_refs <- rbind(shore_quart_hb, shore_quart_sco) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(dataset_type, hb_name) |>
  ungroup()

rm(shore_quart_hb, shore_quart_sco)

#### Get Basic data #######

basic_pt <- read_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_PT.parquet")) |>
  mutate(dataset_type = "PT")
basic_camhs <- read_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_CAMHS.parquet")) |>
  mutate(dataset_type = "CAMHS")

basic_refs <- rbind(basic_pt, basic_camhs) |>
  rename(hb_name = HB, 
         ref_quarter_ending = quarter_ending) |>
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending, "%Y-%m-%d"),
         referrals = as.integer(referrals))

rm(basic_pt, basic_camhs)

#### Bind together ########

comp_quart_refs_hb <- full_join(basic_refs, shore_quart_refs, 
                                by = c("dataset_type" , "hb_name", "ref_quarter_ending"),
                                suffix = c("_basic", "_shore")) |> 
  
  # add change measures
  mutate(difference = referrals_shore - referrals_basic, 
         perc_change = round((referrals_shore - referrals_basic) / referrals_basic * 100, 1))




#### Present neatly ######
all_quart_refs <- comp_quart_refs_hb |>
  #filter(dataset_type == dataset_choice) |>
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending, "%Y-%m-%d"),
         ref_quarter_ending = format(as.Date(ref_quarter_ending), "%b '%y"),
         perc_change = paste0(perc_change, "%"),
         across(c(referrals_basic, referrals_shore, difference), .fns = ~prettyNum(., big.mark = ",")),
         across(c(referrals_basic, referrals_shore, difference), as.character),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  rename(`Referral no. (basic data)` = referrals_basic,
         `Referral no. (optimised data)` = referrals_shore,
         `Difference` = difference,
         `Percent difference` = perc_change,
         `Quarter ending` = ref_quarter_ending,
         `Health Board` = hb_name) |> 
  arrange(`Health Board`) |>
 save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_basic_opti_quarterly")) # _", dataset_choice


latest_quart_refs <- all_quart_refs |>
  #filter(dataset_type == ds_name) |>
  filter(`Quarter ending`  == max(`Quarter ending` )) |>
  select(-`Quarter ending`) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_basic_opti_last_quart")) # _", dataset_choice
