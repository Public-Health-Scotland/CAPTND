#####################################################.
### BASIC v SHOREWISE REFERRALS - for publication ###
#####################################################.

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

shore_quart_refs <- df_shore_ref |>
  distinct() |> 
  group_by(dataset_type, hb_name, ref_quarter_ending) |> 
  summarise(referrals = n(), .groups = "drop") |>
  group_by(dataset_type, ref_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(dataset_type, hb_name) |>
  ungroup()


#### Get Basic data #######
source("./07_publication/investigations/get_basic_data_referrals_df.R")

basic_pt <- get_basic_data_referrals_df("PT") |>
  mutate(dataset_type = "PT")
basic_camhs <- get_basic_data_referrals_df("CAMHS") |>
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
  select(dataset_type, everything()) |>
  
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
  filter(`Quarter ending`  == max(`Quarter ending`)) |>
  select(-`Quarter ending`) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_basic_opti_last_quart")) # _", dataset_choice



###### CREATE EXCEL DOC ########

# Create a blank workbook
apps_b_s <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(apps_b_s, "Basic vs Opti Refs - Quarterly")
addWorksheet(apps_b_s, "Basic vs Opti Refs - Last Qt")

#addWorksheet(apps_b_s, "Sheet 2 Name")

# Write the data to the sheets
writeData(apps_b_s, sheet = "Basic vs Opti Refs - Quarterly", x = all_quart_refs)
writeData(apps_b_s, sheet = "Basic vs Opti Refs - Last Qt", x = latest_quart_refs)


# Export the file
saveWorkbook(apps_b_s, paste0(shorewise_pub_dir, "/measure_summaries/refs_basic_opti_forpub.xlsx"), overwrite = TRUE)
