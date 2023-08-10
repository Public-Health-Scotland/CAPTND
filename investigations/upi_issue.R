x=df_glob_swift_refs2 %>% 
  filter(is.na(chi)) %>% 
  select(chi, upi, sub_source, hb_name, dataset_type, ref_rec_date) %>% 
  distinct() %>% 
  group_by(chi, upi, sub_source, hb_name, dataset_type) %>% 
  summarise(n=n())