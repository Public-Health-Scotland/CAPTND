

apps_df=df_glob_swift_completed %>% 
  select(all_of(c(data_keys,vec_app_cols))) %>% 
  distinct() %>% 
  filter(!is.na(!!sym(app_date_o)))

apps=c(data_keys,app_date_o)

apps_df_dup=apps_df %>% 
  group_by(across(all_of(apps))) %>% 
  summarise(n_apps_same_day=n(), .groups = 'drop') %>% 
  filter(n_apps_same_day>1) 


apps_df_dup_detail=apps_df_dup%>% 
  inner_join(apps_df, by=c(apps))