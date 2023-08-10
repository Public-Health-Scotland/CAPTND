
individualsWithLoadsOfRecs=df_glob_swift_refs2 %>% 
  filter(sub_source=='swift', str_detect(hb_name,'Glasgow')) %>% 
  group_by(across(all_of(data_keys))) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  head(3)

