#Count referrals with discharge dates but no appointments

ref_closed_no_apps <- df_glob_swift_completed %>%
  select(all_of(data_keys), !!app_date_o, !!case_closed_date_o) %>% 
  distinct() %>% 
  filter(is.na(!!sym(app_date_o) & !is.na(!!sym(case_closed_date_o)))
