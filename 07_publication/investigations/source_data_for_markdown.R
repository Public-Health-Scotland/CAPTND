#########################################################################.
### Source data for tables and inline values for publication markdown ###.
#########################################################################.

# Author: Bex Madden
# Date: 9/1/2024


### pull in data tables --------------------------------------------------

#Total referrals data
table_data <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date,   "/shorewise_publication/data/referrals/table_referrals_quarterly.parquet")) |> 
  filter(dataset_type == dataset_choice) |> 
  select(-dataset_type)

# Referral acceptance data
table_data2 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance/table_acc_rate.parquet")) |> 
  ungroup() |> 
  filter(dataset_type == dataset_choice) |> 
  select(-dataset_type) |> 
  relocate(Total, .after = `Health board`)

#Total appointments data
table_data3 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/table_app_att_latest_qt.parquet")) |> 
  filter(dataset_type == dataset_choice) |> 
  select(-dataset_type)

#First contact appointments data
table_data4 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_firstcon/table_firstcon_att_latest_qt.parquet")) |> 
  filter(dataset_type == dataset_choice) |> 
  select(-dataset_type)

# Referrals basic v opti data
table_data5 <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/basic_v_opti/table_refs_basic_opti_last_quart.parquet")) |> 
  filter(dataset_type == dataset_choice) |> 
  select(-dataset_type)

# #Total Appointment DNAs
# table_data5 <- read_parquet(
#   paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/table_tot_dna_rate.parquet")) |>
#   filter(dataset_type == dataset_choice) |> 
#   select(-dataset_type)
  
### Wrangle data for inline values - referrals ----------------------------

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
  filter(dataset_type == dataset_choice)

age_peak <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/age_peak.parquet")) |> 
  filter(dataset_type == dataset_choice)

agesex_total_last_qt <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/agesex_total_last_qt.parquet")) |> 
  filter(dataset_type == dataset_choice)

# age_group <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/age_sex_sumstats/age_groups_", dataset_choice, ".parquet"))

#simd refs figures
figs_simd_refs <- read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals/referrals_quarter_hb_simd.parquet")) |> 
  select(-total) |> 
  filter(dataset_type == dataset_choice,
         quarter_ending == month_end,
         hb_name == "NHS Scotland",
         simd2020_quintile == "1" |
           simd2020_quintile == "5") |> 
  mutate(count = prettyNum(count, big.mark = ","))

# for looked after child
lac_status <- c('Yes', 'No', 'Not known')

ref_lac <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals_by_lac/referrals_lac_qt_hb.parquet")) |>
  filter(dataset_type == dataset_choice,
         ref_quarter_ending == month_end,
         hb_name == "NHS Scotland") |>
  arrange(desc(count)) |>
  group_by(hb_name, ref_quarter_ending) |>   #add_proportion_ds_hb() 
  mutate(total = sum(count),
         prop = round ( count / total * 100 , 1),
         looked_after_c_edited = factor(looked_after_c_edited, levels = lac_status),
         across(everything(), ~prettyNum(., big.mark = ","))) |>
  arrange(looked_after_c_edited)

# for child protection status
cp_status <- c('Yes', 'No', 'Not known', 'Data missing') 

ref_child_prot <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals_by_prot_status/referrals_prot_child_qr_hb.parquet")) |>
  filter(dataset_type == dataset_choice,
         ref_quarter_ending == month_end,
         hb_name == "NHS Scotland") |>
  arrange(desc(count)) |>
  group_by(hb_name, ref_quarter_ending) |>   #add_proportion_ds_hb() 
  mutate(total = sum(count),
         prop = round ( count / total * 100 , 1),
         prot_label = factor(prot_label, levels = cp_status),
         across(everything(), ~prettyNum(., big.mark = ","))) |>
  arrange(prot_label)

# for referral source
ref_source <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/referrals_by_ref_source/ref_source_quarter_hb.parquet")) |>
  filter(dataset_type == dataset_choice,
         quarter_ending == month_end,
         hb_name == "NHS Scotland") |>
  arrange(desc(count)) |> 
  mutate(across(everything(), ~prettyNum(., big.mark = ",")))


# Wrangle data for inline values - referral acceptance -------------------

# for referral acceptance rate
figs_ref_acc <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance/non_acceptance_summary_quarter_hb.parquet")) |> 
  filter(dataset_type == dataset_choice,
         hb_name == "NHS Scotland",
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
  filter(dataset_type == dataset_choice,
         quarter_ending == month_end,
         hb_name == "NHS Scotland",
         ref_rej_reason_desc != "Not recorded at board level") |> 
  arrange(desc(prop)) |>  # arrange by % so can dynamically set most common option
  mutate(across(everything(), ~prettyNum(., big.mark = ","))) 

# for referral non-acceptance action section
ref_rej_actions <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/non_acceptance_action/non_acceptance_action_quarter_hb.parquet")) |> 
  filter(dataset_type == dataset_choice,
         quarter_ending == month_end,
         hb_name == "NHS Scotland",
         ref_rej_act_desc != "Not recorded at board level") |> 
  arrange(desc(prop)) |> # arrange by % so can dynamically set most common option
  mutate(across(everything(), ~prettyNum(., big.mark = ","))) 


# Wrangle data for inline values - appointments ----------------------------
#for DNA rate
# dna_rate <- read_parquet(
#   paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/dnas_total_qr_hb.parquet")) |>
#   filter(!!sym(dataset_type_o) == dataset_choice,
#          hb_name == "NHS Scotland") |>
#   mutate(across(1:4, ~prettyNum(., big.mark = ",")),
#          dna_rate = round(as.numeric(dna_rate)*100,1))
figs_tot_apps <- table_data3 |> 
  filter(`Health board` == "NHS Scotland") |> 
  select(-`Health board`)

# for first contact appointments section
figs_1st_apps <- table_data4 |> 
  filter(`Health board` == "NHS Scotland") |> 
  select(-`Health board`) |> 
  mutate(across(everything(),~ gsub(",", "", .)),
         across(everything(),~ gsub("%", "", .))) |> 
  mutate_all(as.numeric) |> 
  mutate(prop_first_con = round(`1st contact appointments`/`Total appointments`*100, 1),
         across(1:5, ~prettyNum(., big.mark = ",")),
         `1st contact DNA rate` = paste0(`1st contact DNA rate`, "%"),
         prop_first_con = paste0(prop_first_con, "%"))

# for appointment location
appt_loc <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_loc/apps_loc_qt_hb.parquet")) |>
  filter(dataset_type == dataset_choice,
         app_quarter_ending == month_end,
         hb_name == "NHS Scotland") |>
  arrange(desc(count)) |>
  mutate(loc_label = case_when(is.na(loc_label) ~ 'Data missing',
                               TRUE ~ loc_label),
         across(1:4, ~prettyNum(., big.mark = ",")))

appt_loc_missing_fig <- appt_loc |>
  filter(loc_label == 'Data missing')

appt_loc <- appt_loc |>
  filter(loc_label != 'Data missing') #not sure we want to remove data missing altogether? top 5 + 'Not known' category e.g. in ref source, plus 'all other'

# for healthcare professional
prof_group <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_prof/apps_prof_qt_hb.parquet")) |>
  filter(dataset_type == dataset_choice,
         app_quarter_ending == month_end,
         hb_name == "NHS Scotland") |>
  arrange(desc(count)) |>
  mutate(prof_label = case_when(is.na(prof_label) ~ 'Data missing',
                               TRUE ~ prof_label),
         across(1:4, ~prettyNum(., big.mark = ",")))

prof_group_missing_fig <- prof_group |>
  filter(prof_label == 'Data missing')

prof_group <- prof_group |>
  filter(prof_label != 'Data missing') #not sure we want to remove data missing altogether? top 5 + 'Not known' category e.g. in ref source, plus 'all other'

# appointments for last 5 quarters
figs_apps_qt <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_att/apps_att_qt_hb.parquet")) |>
  filter(!!sym(dataset_type_o) == dataset_choice,
         !!sym(hb_name_o) == "NHS Scotland",
         Attendance == "Attended") |>
  select(-c("apps_att", "prop_apps_att")) |>
  pivot_wider(names_from = app_quarter_ending, values_from = total_apps) |>
  select("last_yr" = 4, "last_qt" = 7, "current_qt" = 8) |>
  mutate_all(as.numeric) |>
  mutate(diff_qt = current_qt - last_qt,
         diff_yr = current_qt - last_yr,
         prop_qt = round((diff_qt/last_qt*100), 1),
         prop_yr = round((diff_yr/last_yr*100), 1),
         across(everything(), ~prettyNum(., big.mark = ",")))#WIP??

# for simd dna section
figs_dna_simd <- read_parquet(
  paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_firstcon/apps_firstcon_qt_hb_simd.parquet")) |>
  filter(dataset_type == dataset_choice,
         app_quarter_ending == month_end,
         hb_name == "NHS Scotland",
         Attendance == "Patient DNA",
         !is.na(simd2020_quintile)) |>
  mutate(prop_firstcon_dna = round(firstcon_att/first_contact*100, 1))


# Wrangle data for inline values - basic v opti -----------------------------
# for basic v opti section
figs_basic_opti <- table_data5 |> 
  filter(`Health board` == "NHS Scotland") |> 
  select(-`Health board`) |> 
  mutate(across(everything(),~ gsub("-", "", .))) # just removing - sign


