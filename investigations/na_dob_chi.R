dob_evaluate = df_swift_clean_completed %>% select(dob_from_chi) %>% 
  mutate(valid=case_when(is.na(dob_from_chi) ~ FALSE,
         TRUE ~ TRUE))

table(dob_evaluate$valid)

perc=2908*100/1482245
perc
#0.2% of records have NA for dob from chi

