
read_parquet(df_glob_swift_completed,'../../../output/df_glob_swift_completed.parquet')

patients_with_weird_ages=df_glob_swift_completed %>%  
  append_age_vars() %>% 
  filter(!!sym(age_at_ref_rec_o)==0)
