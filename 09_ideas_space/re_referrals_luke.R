
##########################################
###  Investigate Patient Re-referrals  ###
##########################################

# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# 2 - Identify patients re-referred with referral status of 'accepted' --------------------
reref_year <- df |>
  #filter(!!sym(referral_month_o) >= max(!!sym(referral_month_o)) - months(12)) %>%
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(patient_id_o, ucpn_o) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, patient_id_o))) |>
  filter(ref_acc == 1) |> #accepted referrals
  mutate(count = n()) |>
  ungroup() |>
  filter(count > 1) #filter for patients with more than 1 referral to same board and service (CAMHS or PT)

# 3 - Create df with demographic variables------------------------------
reref_demo <- reref_year |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(sex_reported_o), 
         !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o), count) |>
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


# 4 - Predictive Modelling-------------------------------------------
reref_df <- df |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(ucpn_o), !!sym(ref_acc_o),
         !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o)) |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |>
  ungroup() |>
  arrange(patient_id_o, ucpn_o) |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, patient_id_o))) |>
  filter(ref_acc == 1) |> #accepted referrals
  mutate(count = n()) |>
  ungroup()

reref_df <- reref_df |>
  distinct() |> #remove referrals on same date
  filter(!!sym(sex_reported_o) == 1 | !!sym(sex_reported_o) == 2) |>
  filter(!is.na(!!sym(simd_quintile_o))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(sex_reported = as.factor(sex_reported)) |>
  mutate(simd2020_quintile = as.factor(simd2020_quintile)) #|>
  #mutate(re_referred = case_when(count == 1 ~ 'No',
                                 #count >= 2 ~ 'Yes')) 
#ANOVA

data <- reref_df %>%
  group_by(!!sym(sex_reported_o), !!sym(simd_quintile_o)) %>%
  summarise(mean = mean(count),
            sd = sd(count))

model <- aov(count ~ sex_reported * simd2020_quintile, data = reref_df)

summary(model)

TukeyHSD(model, conf.level=.95)






