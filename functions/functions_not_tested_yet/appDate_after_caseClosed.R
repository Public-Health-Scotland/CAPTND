x=df_glob_merged_cleaned %>% filter(app_date > case_closed_date)

y=df_glob_merged_cleaned %>% filter(ref_rec_date > app_date)

#they're all zero