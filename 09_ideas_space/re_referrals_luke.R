
##########################################
###  Investigate Patient Re-referrals  ###
##########################################

# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# 2 - Identify patients re-referred------------------------------------
reref_df <- df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(ucpn_o), !!sym(rtt_eval_o), 
         !!sym(act_code_sent_date_o), !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o)) |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(patient_id_o, ucpn_o) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, patient_id_o))) |>
  #filter(ref_acc == 1) |> #accepted referrals
  mutate(count = n()) |>
  ungroup() |>
  filter(count > 1) #filter for patients with more than 1 referral to same board and service (CAMHS or PT)
  #does this need a filter to remove digital referrals

# 3 - Create df with demographic variables------------------------------
reref_demo <- reref_df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(act_code_sent_date_o), 
         !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o), count) |>
  distinct() |> #remove referrals on same date
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  slice_head(n = 1) |>
  ungroup()

#re-referrals distribution
reref_dist <- reref_demo |>
  group_by(!!sym(dataset_type_o), count) |>
  summarise(n = n())

#re-referrals by sex
reref_sex <- reref_demo |>
  select(!!sym(dataset_type_o), !!sym(patient_id_o), !!sym(sex_reported_o)) |>
  group_by(!!sym(dataset_type_o), !!sym(sex_reported_o)) |>
  summarise(n = n())

#re-referral by age-group
reref_age <- reref_demo |>
  select(!!sym(dataset_type_o), !!sym(patient_id_o), !!sym(age_group_o)) |>
  group_by(!!sym(dataset_type_o), !!sym(age_group_o)) |>
  summarise(n = n())

#re-referral by SIMD
reref_simd <- reref_demo |>
  select(!!sym(dataset_type_o), !!sym(patient_id_o), !!sym(simd_quintile_o)) |>
  group_by(!!sym(dataset_type_o), !!sym(simd_quintile_o)) |>
  summarise(n = n())


# 4 - Time Between Referrals-----------------------------------------
reref_time <- reref_df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(sex_reported_o), 
         !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(rtt_eval_o), !!sym(ref_rec_date_o), count) |>
  distinct() |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  arrange(!!sym(ref_rec_date_o), .by_group = TRUE) |>
  mutate(lag_col = lag(ref_rec_date)) |>
  mutate(days_between_refs = as.numeric(ref_rec_date - lag_col)) |>
  #filter(!is.na(days_between_refs)) |>
  ungroup() |>
  #filter(!!sym(hb_name_o) == 'NHS Highland') |>
  mutate(reref_period = case_when(days_between_refs <= 168 ~ '0-6 Months',
                                  days_between_refs > 168 & days_between_refs <= 365 ~ '7-12 Months',
                                  days_between_refs > 365 & days_between_refs <= 533 ~ '13-18 Months',
                                  days_between_refs > 533 & days_between_refs <= 730 ~ '19-24 Months',
                                  days_between_refs > 730 & days_between_refs <= 898 ~ '25-30 Months',
                                  days_between_refs > 898 & days_between_refs <= 1095 ~ '31-36 Months',
                                  days_between_refs > 1095 ~ '37+ Months'))

#Count of rtt_eval for first referral
first_ref <- reref_time |>
  filter(is.na(days_between_refs)) |>
  group_by(!!sym(dataset_type_o), !!sym(rtt_eval_o)) |>
  summarise(count = n())

#Count of rtt_eval of first referral for patients re-referred within 6 months
first_ref_0_6mths <- reref_time |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  filter(any(reref_period == '0-6 Months') &
           is.na(days_between_refs)) |>
  group_by(!!sym(dataset_type_o), !!sym(rtt_eval_o)) |>
  summarise(count = n()) |>
  group_by(!!sym(dataset_type_o)) |>
  mutate(tot = sum(count),
         prop = round(count/tot * 100, 1))
  

df_time <- reref_time |> 
  mutate(reref_period = factor(reref_period, levels=c('0-6 Months', '7-12 Months', '13-18 Months',
                                                         '19-24 Months', '25-30 Months', '31-36 Months',
                                                         '37+ Months'))) |>
  group_by(reref_period) |>
  summarise(count = n())


# 5 - Stats Modelling-------------------------------------------
comp_ref_df <- df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(ucpn_o), !!sym(ref_acc_o),
         !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o)) |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(patient_id_o, ucpn_o) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, patient_id_o))) |>
  mutate(count = n()) |>
  ungroup() 
  
anova_df <- comp_ref_df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(sex_reported_o), 
         !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o), count) |>
  distinct() |> #remove referrals on same date
  filter(!!sym(dataset_type_o) == 'PT', #choose ds type
           !!sym(sex_reported_o) %in% c(1,2) &
           !is.na(!!sym(simd_quintile_o)) &
           !is.na(!!sym(age_group_o))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(sex_reported = as.factor(sex_reported),
         simd2020_quintile = as.factor(simd2020_quintile),
         age_group = as.factor(age_group)) |>
  mutate(re_referred = case_when(count == 1 ~ 'No',
                                 count >= 2 ~ 'Yes')) 

#ANOVA

data <- anova_df |>
  filter(!!sym(dataset_type_o) == 'PT') |>
  group_by(!!sym(sex_reported_o), !!sym(simd_quintile_o), !!sym(age_group_o)) %>%
  summarise(mean = mean(count),
            sd = sd(count))

model <- aov(count ~ sex_reported * simd2020_quintile * age_group, data = anova_df)

summary(model)

TukeyHSD(model, conf.level=.95) # must be factors for this






