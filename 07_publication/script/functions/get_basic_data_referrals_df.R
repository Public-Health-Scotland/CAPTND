###############################################.
#### Publication - basic data referrals df ####.
###############################################.

# Author: Bex Madden
# Date: 2024-06-13

get_basic_data_referrals_df <- function(dataset_choice){
  
# get file location of latest captnd data extract
# filepath <- "../../../../R script/CAPTND Data Prep/Output/"
# 
# files <- as.Date(list.files(filepath))
# latest_file <- max(files, na.rm = TRUE)

# 1 - Load latest CAPTND data ---------------------------------------------------------

#df <-  read_parquet(file = paste0(data_prep_dir, "/captnd_raw.parquet")) %>%
df <- read_parquet(file = paste0(data_prep_dir, "/assess_refs.parquet")) %>%
  filter(!is.na(ucpn),
         header_date %in% date_range) %>%
  distinct(.)
  
# df2 <-  read_parquet(file = paste0(filepath, latest_file, "/all_records.parquet")) %>%
#   filter(!is.na(UCPN),
#          rec_month %in% date_range) %>% # filter by date range)
#   distinct(.)

df_all <- df |>
  filter(dataset_type == dataset_choice)

# df_all <- df |>
#   filter(DATASET == dataset_choice)


# 2 - wrangle data --------------------------------------------------------

# Add Derived DOB and Age ---------------------------------------------
# df_test <- df_all %>% 
#   mutate(age_ref = round(lubridate::interval(dob, ref_date) / years(1), digits = 0)) %>% 
#   mutate(age_ref = if_else(age_ref >= 0 & age_ref <= 110 , age_ref, NA_real_), # use plausible ages
#          age_ref = if_else(dataset_type == "CAMHS" & age_ref > 20, NA_real_, age_ref)) # use plausible ages for CAMHS


# df_test <- df_all %>% 
#   mutate(age_ref = round(lubridate::interval(DOB, RECEIVED_DATE) / years(1), digits = 0)) %>% 
#   mutate(age_ref = if_else(age_ref >= 0 & age_ref <= 110 , age_ref, NA_real_), # use plausible ages
#          age_ref = if_else(DATASET == "CAMHS" & age_ref > 20, NA_real_, age_ref)) # use plausible ages for CAMHS


# Add Age Groups (tbc) ------------------------------------------------ 
# df_test <- df_test %>%  
#   mutate(age_group = if(dataset_choice == "CAMHS") {
#     case_when(
#       age_ref >= 0 & age_ref <= 4 ~ "0-4",
#       age_ref >= 5 & age_ref <= 12 ~ "5-12",
#       age_ref >= 13 & age_ref <= 16 ~ "13-16",
#       age_ref >= 17 & age_ref <= 25 ~ "17+")
#   } else {
#     case_when(
#       age_ref >= 0 & age_ref <= 4 ~ "0-4",
#       age_ref >= 5 & age_ref <= 11 ~ "5-11",
#       age_ref >= 12 & age_ref <= 18 ~ "12-18",
#       age_ref >= 19 & age_ref <= 40 ~ "19-40",
#       age_ref >= 41 & age_ref <= 64 ~ "41-64",
#       age_ref >= 65 & age_ref <= 79 ~ "65-79",
#       age_ref >= 80 ~ "80+")}) 
# 
# df_test <- df_test %>% 
#   mutate(age_group = factor(age_group, 
#                             levels = if(dataset_choice == "CAMHS"){
#                               c("0-4", "5-12", "13-16", "17+") 
#                             } else {
#                               c("0-4", "5-11", "12-18", "19-40", "41-64", "65-79", "80+")})) 


# Rename SIMD Quintile ------------------------------------------------

# df_test <- df_test %>% 
#   rename(SIMD2020_5 = SIMD20_quintile) %>% 
#   mutate(SIMD2020_5 = as.character(SIMD2020_5)) 


# 3 - referral trend -----------------------------------------------------------

# HB quarterly trend table 
table_hb_q_trend <- df_all %>%
    mutate(quarter = ceiling_date(ref_rec_date, unit = "quarter") - 1,
         quarter_ending = floor_date(quarter, unit = "month")) %>% 
  group_by(chi, ucpn) %>% 
  slice(1) %>% 
  ungroup() %>%
  group_by(hb_name, quarter_ending) %>% 
  summarise(referrals = n(), .groups = "drop") %>% 
  group_by(quarter_ending)  %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) %>%
  select(c("hb_name", everything())) %>%
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) %>% 
  filter(!is.na(hb_name)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_", dataset_choice))

# monthly
table_hb_m_trend <- df_all %>%
  group_by(chi, ucpn) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(ref_rec_month = floor_date(ref_rec_date, unit = "month")) %>% 
  group_by(hb_name, ref_rec_month) %>% 
  summarise(referrals = n(), .groups = "drop") %>% 
  group_by(ref_rec_month)  %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) %>%
  select(c("hb_name", everything())) %>%
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) %>% 
  filter(!is.na(hb_name)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_monthly_", dataset_choice))

# return(table_hb_q_trend)
# return(table_hb_m_trend) # load in from file instead
}