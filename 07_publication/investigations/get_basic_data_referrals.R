######################################################.
#### GET BASIC REFERRALS COUNTS - for publication ####.
######################################################.

source('02_setup/save_df_as_parquet.R')

month_end <- ymd("2024-03-31")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")


filepath <- "../../../../R script/CAPTND Data Prep/Output/"

files <- as.Date(list.files(filepath))
latest_file <- max(files, na.rm = TRUE)


dataset_choice = "PT"


# 1 - CAPTND data ---------------------------------------------------------

df <-  read_parquet(file = paste0(filepath, latest_file, "/all_records.parquet")) %>% 
  filter(!is.na(UCPN),
         rec_month >= month_start & 
           rec_month <= month_end) %>% 
  distinct(.) 

df_all <- df |>
  filter(DATASET == dataset_choice)
# 2 - Basefiles  ----------------------------------------------------------

#### NEEDED?? #####
loc_backdata <- "../../../../R script/CAPTND Data Prep/Data/Basefiles/"

# Get health board code lookup
hb_codes <- readxl::read_xlsx(paste0(loc_backdata,"hb_codes.xlsx"), sheet = "hb_codes") %>% 
  select(-c(3:5))

# Get population estimates
pop <- readxl::read_xlsx(paste0(loc_backdata,"hb2019_pop_est.xlsx"), sheet = "estimates") %>% 
  filter(Year == max(Year) &
           Sex == "All") %>% 
  pivot_longer(cols = c(6:97), names_to = "Age", values_to = "Count") %>% 
  left_join(. , hb_codes, by = "HB") %>% 
  mutate(HBName = if_else(is.na(HBName), "NHS Scotland", HBName)) %>% 
  select(Year, HB, HBName, everything()) %>% 
  select(-c("HBQF", "SexQF")) %>% 
  filter(Age != "AllAges")

pop_age_SIMD <- readxl::read_xlsx(paste0(loc_backdata,"pop_age_SIMD.xlsx")) %>% 
  tidyr::pivot_longer(cols = c(2:92), names_to = "Age", values_to = "Count")

ref_source_93 <- readxl::read_xlsx(paste0(loc_backdata,"Lookup.xlsx"), sheet = "Ref_Source") %>% 
  mutate(REF_SOURCE = as.character(Ref_Source)) %>% 
  mutate(REF_SOURCE = if_else(nchar(REF_SOURCE) == 1, paste0("0", REF_SOURCE), REF_SOURCE))


ages_camhs <- c("Age0", "Age1", "Age2", "Age3", "Age4", "Age5", "Age6", "Age7", "Age8", "Age9", "Age10", "Age11", 
                "Age11", "Age12", "Age13", "Age14", "Age15", "Age16", "Age17", "Age18"#, "Age19", "Age20"
)


if (dataset_choice == "CAMHS"){
  pop_ages <- pop %>% 
    filter(Age %in% ages_camhs) %>% # use ages 1:18
    group_by(HBName) %>% 
    summarise(pop = sum(Count, na.rm = TRUE)) %>% 
    rename(HB = HBName)
  
} else{
  pop_ages <- pop %>% 
    group_by(HBName) %>% 
    summarise(pop = sum(Count, na.rm = TRUE)) %>% 
    rename(HB = HBName)
}


if (dataset_choice == "CAMHS"){
  pop_SIMD <- pop_age_SIMD %>% 
    filter(Age %in% ages_camhs) %>% 
    group_by(SIMD2020_5) %>% 
    summarise(pop = sum(Count, na.rm = TRUE)) 
  
} else{
  pop_SIMD <- pop_age_SIMD %>% 
    group_by(SIMD2020_5) %>% 
    summarise(pop = sum(Count, na.rm = TRUE)) 
}




# 2 - wrangle data --------------------------------------------------------

# df_test <- df_all %>% 
#   mutate(derived_sex = case_when(
#     odd(as.numeric(substr(CHI,9,9))) == TRUE ~ "01", 
#     even(as.numeric(substr(CHI,9,9))) == TRUE ~ "02",
#     TRUE ~ NA_character_))


# Add Derived DOB and Age ---------------------------------------------
df_test <- df_all %>% 
  mutate(age_ref = round(lubridate::interval(DOB, RECEIVED_DATE) / years(1), digits = 0)) %>% 
  mutate(age_ref = if_else(age_ref >= 0 & age_ref <= 110 , age_ref, NA_real_), # use plausible ages
         age_ref = if_else(DATASET == "CAMHS" & age_ref > 20, NA_real_, age_ref)) # use plausible ages for CAMHS


# Add Age Groups (tbc) ------------------------------------------------ 
df_test <- df_test %>%  
  mutate(age_group = if(dataset_choice == "CAMHS") {
    case_when(
      age_ref >= 0 & age_ref <= 4 ~ "0-4",
      age_ref >= 5 & age_ref <= 12 ~ "5-12",
      age_ref >= 13 & age_ref <= 16 ~ "13-16",
      age_ref >= 17 & age_ref <= 25 ~ "17+")
  } else {
    case_when(
      age_ref >= 0 & age_ref <= 4 ~ "0-4",
      age_ref >= 5 & age_ref <= 11 ~ "5-11",
      age_ref >= 12 & age_ref <= 18 ~ "12-18",
      age_ref >= 19 & age_ref <= 40 ~ "19-40",
      age_ref >= 41 & age_ref <= 64 ~ "41-64",
      age_ref >= 65 & age_ref <= 79 ~ "65-79",
      age_ref >= 80 ~ "80+")}) 

df_test <- df_test %>% 
  mutate(age_group = factor(age_group, 
                            levels = if(dataset_choice == "CAMHS"){
                              c("0-4", "5-12", "13-16", "17+") 
                            } else {
                              c("0-4", "5-11", "12-18", "19-40", "41-64", "65-79", "80+")})) 


# Rename SIMD Quintile ------------------------------------------------

df_test <- df_test %>% 
  rename(SIMD2020_5 = SIMD20_quintile) %>% 
  mutate(SIMD2020_5 = as.character(SIMD2020_5)) 




# 3 - check data ----------------------------------------------------------

HB = unique(df_test$HB)
HB

rec_month = seq.Date(from = month_start, to = month_end, by = "months")
no_months <- length(rec_month)

#df_hb_months <- left_join(df_hb2, df_months, by = character())

dodgy_subs <- df_test %>% 
  group_by(UCPN) %>% 
  slice(1) %>% 
  dplyr::group_by(HB, rec_month) %>% 
  dplyr::summarise(referrals = n_distinct(UCPN, na.rm = TRUE)) %>% 
  group_by(HB) %>% 
  mutate(n_subs = max(row_number())) %>% 
  filter(n_subs != no_months)

# # ID missing sub months
# missing_subs <- anti_join(df_hb_months, dodgy_subs,  by = c("HB", "rec_month")) %>% 
#   group_by(HB) %>% 
#   mutate(missed_subs = max(row_number())) %>% 
#   filter(missed_subs != no_months) %>% 
#   arrange(desc(HB))
# 
# missing_subs_hbs <- missing_subs %>% 
#   select(HB) %>% 
#   unique() %>% 
#   pull(.)


rm(HB, df_hb2, rec_month, df_months, df_hb_months, dodgy_subs)



# check monthly counts and submissions
check_refs <- df_test %>% 
  group_by(UCPN) %>% 
  slice(1) %>% 
  ungroup() %>%
  group_by(HB, rec_month) %>% 
  summarise(referrals = n()) %>% 
  ungroup() %>% 
  group_by(HB) %>% 
  mutate(subs = n()) %>% 
  ungroup() %>% 
  filter(subs != max(subs)) # look at HBs with missed subs




# 4 - referral trend -----------------------------------------------------------

# HB quarterly trend table 
refs_tab_hb <- df_test %>%
  mutate(quarter = ceiling_date(RECEIVED_DATE, unit = "quarter") - 1,
         quarter_ending = floor_date(quarter, unit = "month")) %>% 
  group_by(UCPN) %>% 
  slice(1) %>% 
  ungroup() %>%
  group_by(HB, quarter_ending) %>% 
  summarise(referrals = n())


refs_tab_sco <- refs_tab_hb %>% 
  group_by(quarter_ending) %>% 
  summarise(referrals = sum(referrals, na.rm = TRUE)) %>% 
  mutate(HB = "NHS Scotland") %>% 
  select(c("HB", everything()))


table_hb_q_trend <- rbind(refs_tab_hb, refs_tab_sco) %>%
  arrange(quarter_ending) %>% 
  pivot_wider(names_from = quarter_ending, values_from = referrals) %>% 
  mutate(HB = factor(HB, levels = level_order_hb)) %>% 
  mutate_all(~replace(., is.na(.), "..")) |>
  filter(!is.na(HB)) 


# make quarterly trend long
table_hb_q_trend_long <- table_hb_q_trend |> 
  pivot_longer(cols = 2:6, names_to = "quarter_ending", values_to = "referrals") |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/basic_refs_quarterly_", dataset_choice))
