#patients with ref date equal to dob

x=read_parquet('../../../output/df_glob_swift_completed.parquet')

patients_with_weird_ages=x %>%  
  append_age_vars() %>% 
  filter(!!sym(age_at_ref_rec_o)==0) %>% 
  select(hb_name,dataset_type,patient_id,dob,dob_from_chi,dob_verified,dob_recorded_matches_chi,age_at_ref_rec, age_group,ref_date,ref_rec_date) %>% 
  distinct() %>% 
  filter(ref_rec_date<=dob_verified)