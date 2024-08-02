#####################################################.
### BASIC v SHOREWISE REFERRALS - for publication ###
#####################################################.

# Author: Bex Madden
# Date: 2024-05-28


summarise_referrals_basic_opti <- function(){ #dataset_choice
  
  # create dir for saving output files in
  basic_opti_dir <- paste0(shorewise_pub_data_dir, "/basic_v_opti/")
  dir.create(basic_opti_dir)

# Get Shorewise data -----------------------------------------------------

df_shore <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) 


df_shore_ref <- df_shore |>
  filter(referral_month %in% date_range) |>
  group_by(dataset_type, hb_name, ucpn, patient_id) |> 
  slice(1) |> 
  ungroup() |> 
  select(all_of(data_keys), ref_rec_date, referral_month, ref_quarter_ending) |> 
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending)) |>
  filter(!is.na(ref_rec_date)) 


# Get Basic data --------------------------------------------------------

get_basic_data_referrals_df("PT")
get_basic_data_referrals_df("CAMHS")


# 1. QUARTERLY ----------------------------------------------------------

# make quarterly shorewise referral counts 
shore_quart_refs <- df_shore_ref |>
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


# get quarterly basic referrals counts
basic_pt <-  read_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_PT.parquet")) |>
  mutate(dataset_type = "PT")
basic_camhs <- read_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_CAMHS.parquet")) |>
  mutate(dataset_type = "CAMHS")

basic_refs <- rbind(basic_pt, basic_camhs) |>
  rename(hb_name = HB, 
         ref_quarter_ending = quarter_ending) |>
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending, "%Y-%m-%d"),
         referrals = as.integer(referrals))

rm(basic_pt, basic_camhs)


# bind basic and shorewise together
comp_quart_refs_hb <- full_join(basic_refs, shore_quart_refs, 
                                by = c("dataset_type" , "hb_name", "ref_quarter_ending"),
                                suffix = c("_basic", "_shore")) |> 
  select(dataset_type, everything()) |>
  
  # add change measures
  mutate(difference = referrals_shore - referrals_basic, 
         perc_change = round((referrals_shore - referrals_basic) / referrals_basic * 100, 1))


# Present all quarters
all_quart_refs <- comp_quart_refs_hb |>
  mutate(ref_quarter_ending = as.Date(ref_quarter_ending, "%Y-%m-%d"),
         perc_change = paste0(perc_change, "%"),
         across(c(referrals_basic, referrals_shore, difference), .fns = ~prettyNum(., big.mark = ",")),
         across(c(referrals_basic, referrals_shore, difference), as.character),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  rename(`Referral count (submitted)` = referrals_basic,
         `Referral count (pathways in CAPTND)` = referrals_shore,
         `Numeric difference` = difference,
         `Percent difference` = perc_change,
         `Quarter ending` = ref_quarter_ending) |> 
  arrange(hb_name) |>
  ungroup() |> 
 save_as_parquet(paste0(basic_opti_dir, "refs_basic_opti_quarterly")) 


# 2. MONTHLY ----------------------------------------------------------

# Get monthly shorewise referral counts 
shore_month_refs <- df_shore_ref |>
  distinct() |> 
  group_by(dataset_type, hb_name, referral_month) |> 
  summarise(referrals = n(), .groups = "drop") |>
  group_by(dataset_type, referral_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(dataset_type, hb_name) |>
  ungroup()

# Get monthly basic referral counts
basic_pt_m <- read_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_monthly_PT.parquet")) |>
  mutate(dataset_type = "PT")
basic_camhs_m <- read_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_monthly_CAMHS.parquet")) |>
  mutate(dataset_type = "CAMHS")

basic_refs_m <- rbind(basic_pt_m, basic_camhs_m) |>
  rename(hb_name = HB, 
         referral_month = rec_month) |>
  mutate(referral_month = as.Date(referral_month, "%Y-%m-%d"),
         referrals = as.integer(referrals))

rm(basic_pt_m, basic_camhs_m)


# Bind together 
comp_month_refs_hb <- full_join(basic_refs_m, shore_month_refs, 
                                by = c("dataset_type" , "hb_name", "referral_month"),
                                suffix = c("_basic", "_shore")) |> 
  select(dataset_type, everything()) |>
  
  # add change measures
  mutate(difference = referrals_shore - referrals_basic, 
         perc_change = round((referrals_shore - referrals_basic) / referrals_basic * 100, 1))


# Present for all months

all_month_refs <- comp_month_refs_hb |>
  mutate(referral_month = as.Date(referral_month, "%Y-%m-%d"),
         referral_month = format(as.Date(referral_month), "%b '%y"),
         perc_change = paste0(perc_change, "%"),
         across(c(referrals_basic, referrals_shore, difference), .fns = ~prettyNum(., big.mark = ",")),
         across(c(referrals_basic, referrals_shore, difference), as.character),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  rename(`Referral no. (basic data)` = referrals_basic,
         `Referral no. (optimised data)` = referrals_shore,
         `Difference` = difference,
         `Percent difference` = perc_change,
         `Month` = referral_month,
         `Health board` = hb_name) |> 
  arrange(`Health board`) |>
  save_as_parquet(paste0(basic_opti_dir, "refs_basic_opti_monthly")) 

}