#########################################################################.
### Source data for tables and inline values for publication markdown ###.
#########################################################################.

# Author: Bex Madden
# Date: 9/1/2024



#Total referrals data
table_data <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date,   "/shorewise_publication/data/referrals/table_referrals_quarterly.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice) |> 
  select(-dataset_type)

# Referral acceptance data
table_data2 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance/table_acc_rate.parquet")) |> 
  ungroup() |> 
  filter(!!sym(dataset_type_o) == dataset_choice) |> 
  select(-dataset_type) |> 
  relocate(Total, .after = `Health board`)

#Appointments data
table_data3 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/table_apps_att_latest_qt.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice) |> 
  select(-dataset_type)

# Referrals basic v opti data
table_data4 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/basic_v_opti/table_refs_basic_opti_last_quart.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice) |> 
  select(-dataset_type)


#Data for inline values - referrals

# for total referrals section
figs_referrals <- table_data |> 
  filter(`Health board` == "NHS Scotland") |> 
  select("last_yr" = 2, "last_qt" = 5, "current_qt" = 6) |> 
  mutate(across(everything(),~ gsub(",", "", .))) |> 
  mutate_all(as.numeric) |> 
  mutate(diff_qt = current_qt - last_qt,
         diff_yr = current_qt - last_yr,
         prop_qt = round((diff_qt/last_qt*100), 1),
         prop_yr = round((diff_yr/last_yr*100), 1),
         across(1:5, ~prettyNum(., big.mark = ",")))

# age and sex ref figures (pre-made just read in and filter ds type)
agesex_total <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/agesex_total.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice)

age_peak <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/age_peak.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice)

agesex_total_last_qt <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/agesex_total_last_qt.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice)

# age_group <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/age_groups_", dataset_choice, ".parquet"))

#simd refs figures
figs_simd_refs <- read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/referrals_quarter_hb_simd.parquet")) |> 
  select(-total) |> 
  filter(!!sym(dataset_type_o) == dataset_choice,
         quarter_ending == month_end,
         !!sym(hb_name_o) == "NHS Scotland",
         !!sym(simd_quintile_o) == "1" |
           !!sym(simd_quintile_o) == "5") |> 
  mutate(count = prettyNum(count, big.mark = ","))

# Data for inline values - referral acceptance
# for referral acceptance rate
figs_ref_acc <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance/non_acceptance_summary_quarter_hb.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice,
         !!sym(hb_name_o) == "NHS Scotland",
         ref_acc_desc == "Referral accepted") |> 
  select(-c("total", "count")) |> 
  pivot_wider(names_from = quarter_ending, values_from = prop) |> 
  select("last_yr" = 4, "last_qt" = 7, "current_qt" = 8) |> 
  mutate_all(as.numeric) |> 
  mutate(diff_qt = current_qt - last_qt,
         diff_yr = current_qt - last_yr,
         across(everything(), ~prettyNum(., big.mark = ",")))#WIP??

# for referral non-acceptance reason section
ref_rej_reasons <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance_reason/non_acceptance_reason_quarter_hb.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice,
         quarter_ending == month_end,
         !!sym(hb_name_o) == "NHS Scotland",
         ref_rej_reason_desc != "Not recorded at board level") |> 
  arrange(desc(prop)) |>  # arrange by % so can dynamically set most common option
  mutate(across(everything(), ~prettyNum(., big.mark = ","))) 

# for referral non-acceptance action section
ref_rej_actions <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance_action/non_acceptance_action_quarter_hb.parquet")) |> 
  filter(!!sym(dataset_type_o) == dataset_choice,
         quarter_ending == month_end,
         !!sym(hb_name_o) == "NHS Scotland",
         ref_rej_act_desc != "Not recorded at board level") |> 
  arrange(desc(prop)) # arrange by % so can dynamically set most common option


# Data for inline values - appointments
# for appointments section
figs_apps <- table_data3 |> 
  filter(`Health board` == "NHS Scotland") |> 
  select(-`Health board`) |> 
  mutate(across(everything(),~ gsub(",", "", .)),
         across(everything(),~ gsub("%", "", .))) |> 
  mutate_all(as.numeric) |> 
  mutate(prop_first_con = round(`1st contact appointments`/`Total appointments`*100, 1),
         across(1:5, ~prettyNum(., big.mark = ",")))

# appointments for last 5 quarters
# figs_apps_qt <- read_parquet(
#   paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/apps_att_qt_hb.parquet")) |> 
#   filter(!!sym(dataset_type_o) == dataset_choice,
#          !!sym(hb_name_o) == "NHS Scotland",
#          Attendance == "Attended") |> 
#   select(-c("firstcon_att", "prop_firstcon_att", "first_contact")) |> 
#   pivot_wider(names_from = app_quarter_ending, values_from = total_apps) |> 
#   select("last_yr" = 4, "last_qt" = 7, "current_qt" = 8) |> 
#   mutate_all(as.numeric) |> 
#   mutate(diff_qt = current_qt - last_qt,
#          diff_yr = current_qt - last_yr,
#          prop_qt = round((diff_qt/last_qt*100), 1),
#          prop_yr = round((diff_yr/last_yr*100), 1),
#          across(everything(), ~prettyNum(., big.mark = ",")))#WIP??

# for simd dna section
# figs_dna_simd <- read_parquet(
#   paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/apps_att_qt_hb_simd.parquet")) |> 
#   filter(!!sym(dataset_type_o) == dataset_choice,
#          app_quarter_ending == month_end,
#          !!sym(hb_name_o) == "NHS Scotland",
#          Attendance == "Patient DNA",
#          !is.na(!!sym(simd_quintile_o))) |> 
#   mutate(prop_firstcon_dna = round(firstcon_att/first_contact*100, 1))


# Data for inline values - basic v opti
# for basic v opti section
figs_basic_opti <- table_data4 |> 
  filter(`Health board` == "NHS Scotland") |> 
  select(-`Health board`) |> 
  mutate(across(everything(),~ gsub("-", "", .))) # just removing - sign


