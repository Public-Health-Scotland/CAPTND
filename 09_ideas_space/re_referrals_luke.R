
##########################################
###  Investigate Patient Re-referrals  ###
##########################################

# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# 2 - Identify patients re-referred in last 12 months--------------------
reref_year <- df %>%
  filter(!!sym(referral_month_o) >= max(!!sym(referral_month_o)) - months(12)) %>%
  group_by(across(all_of(data_keys))) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(patient_id_o, ucpn_o) %>%
  group_by(!!!syms(c(dataset_type_o, hb_name_o, patient_id_o))) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 1)

# 3 - Create df with demographic variables------------------------------
reref_demo <- reref_year %>%
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o), !!sym(sex_reported_o), 
         !!sym(age_group_o), !!sym(simd_quintile_o), !!sym(ref_rec_date_o), count) %>%
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) %>%
  filter(!!sym(ref_rec_date_o) == max(!!sym(ref_rec_date_o))) %>%
  distinct() %>%
  #flag duplicated patient_ids
  mutate(flag = +(n() > 1)) %>%
  ungroup()


#re-referrals distribution
reref_dist <- reref_demo %>%
  select(!!sym(dataset_type_o), !!sym(patient_id_o), count) %>%
  distinct() %>%
  group_by(!!sym(dataset_type_o), count) %>%
  summarise(n = n())

#re-referrals by sex
reref_sex <- reref_demo %>%
  select(!!sym(dataset_type_o), !!sym(patient_id_o), !!sym(sex_reported_o)) %>%
  distinct() %>%
  group_by(!!sym(dataset_type_o), !!sym(sex_reported_o)) %>%
  summarise(n = n())

#re-referral by age-group
reref_age <- reref_demo %>%
  select(!!sym(dataset_type_o), !!sym(patient_id_o), !!sym(age_group_o)) %>%
  distinct() %>%
  group_by(!!sym(dataset_type_o), !!sym(age_group_o)) %>%
  summarise(n = n())

#re-referral by SIMD
reref_simd <- reref_demo %>%
  select(!!sym(dataset_type_o), !!sym(patient_id_o), !!sym(simd_quintile_o)) %>%
  distinct() %>%
  group_by(!!sym(dataset_type_o), !!sym(simd_quintile_o)) %>%
  summarise(n = n())

# 4 - Calculate non-accepted referrals--------------------------------
#non-accepted referrals
non_accept <- reref_year %>%
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o),
         !!sym(ref_rec_date_o), !!sym(ref_acc_o), !!sym(ref_rej_reason_o),
         !!sym(ref_rej_act_o), count) %>%
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(patient_id_o)) %>%
  #filter out most recent referral
  filter(!!sym(ref_rec_date_o) < max(!!sym(ref_rec_date_o))) %>%
  filter(!!sym(ref_acc_o) == 2) %>%
  ungroup()

#reason for rejected referral
rej_ref_lookup <- read_xlsx("../../../data/captnd_codes_lookup.xlsx",sheet = 'Rej_Reason') %>%
  select(code = REJ_REASON, refer_rej_reason = Rej_Reason)

rej_ref <- non_accept %>%
  left_join(rej_ref_lookup, by = join_by(!!sym(ref_rej_reason_o) == code)) 

#rejected referral action
rej_ref_action_lookup <- read_xlsx("../../../data/captnd_codes_lookup.xlsx",sheet = 'Rej_Action') %>%
  select(code = REJ_ACTIONS, refer_rej_action = Rej_Action)

rej_ref <- rej_ref %>%
  left_join(rej_ref_action_lookup, by = join_by(!!sym(ref_rej_act_o) == code)) %>%
  select(!!sym(dataset_type_o), !!sym(hb_name_o), refer_rej_reason, refer_rej_action)

# 5 - Graph Rejected Referral Reason/Action---------------------------

rej_ref_reason <- rej_ref %>%
  filter(!!sym(dataset_type_o) == 'PT') %>%
  group_by(refer_rej_reason) %>%
  summarise(count = n())
  
rej_ref_action <- rej_ref %>%
  filter(!!sym(dataset_type_o) == 'PT') %>%
  group_by(refer_rej_action) %>%
  summarise(count = n())




  