##########################################
###  Investigate Patient Re-referrals  ###
##########################################

#Author: Luke Taylor
#Date: 23/02/2026

# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# 2 - Identify patients re-referred------------------------------------
re_referrals_df <- df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(ucpn_o), !!sym(ref_rec_date_opti_o), !!sym(ref_acc_o),
         !!sym(act_code_sent_date_o), !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(rtt_eval_o)) |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |> ungroup() |>
  arrange(patient_id_o) |>
  filter(is.na(!!sym(act_code_sent_date_o)), #remove digi referrals
         ref_acc == 1) |> #only keep accepted referrals
  group_by(!!!syms(c(dataset_type_o, hb_name_o, patient_id_o))) |>
  mutate(count = n()) |> ungroup() |>
  filter(count > 1) |> #filter for patients with more than 1 referral to same board and service
  distinct()

# 3 - Time between referrals-----------------------------------------
time_between_refs <- re_referrals_df |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(ref_rec_date_opti_o)) |>
  mutate(first_ref = min(ref_rec_date_opti),
         days_between_refs = as.numeric(difftime(!!sym(ref_rec_date_opti_o), first_ref, units = 'days'))) |>
  ungroup() |>
  mutate(reref_period = case_when(days_between_refs <= 168 ~ '0-6 Months',
                                  days_between_refs > 168 & days_between_refs <= 365 ~ '7-12 Months',
                                  days_between_refs > 365 & days_between_refs <= 533 ~ '13-18 Months',
                                  days_between_refs > 533 & days_between_refs <= 730 ~ '19-24 Months',
                                  days_between_refs > 730 & days_between_refs <= 898 ~ '25-30 Months',
                                  days_between_refs > 898 & days_between_refs <= 1095 ~ '31-36 Months',
                                  days_between_refs > 1095 ~ '37+ Months'))

time_between_refs$reref_period <- factor(time_between_refs$reref_period, 
                                 levels = c('0-6 Months','7-12 Months','13-18 Months','19-24 Months',
                                 '25-30 Months','31-36 Months','37+ Months'))

hb_summ <- time_between_refs |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  slice(2) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), reref_period) |>
  summarise(count = n()) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), reref_period) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  mutate(tot = sum(count),
         perc = round(count/tot*100, 1))


#Count of rtt_eval for first referral
rtt_first_ref <- time_between_refs |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  slice(1) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(rtt_eval_o)) |>
  summarise(count = n())

#Count of rtt_eval of first referral for patients re-referred within 6 months
rtt_first_ref <- time_between_refs |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  mutate(ref_num = row_number(),
         reref_period = case_when(reref_period == '0-6 Months' & ref_num == 1 ~ NA_character_,
                                  TRUE ~ reref_period)) |>
  filter(any(reref_period == '0-6 Months')) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  slice(1) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(rtt_eval_o)) |>
  summarise(count = n())


  
  
