
df_glob_swift_refs2 <- read_parquet('../../../output/df_glob_swift_refs2.parquet')

y=df_glob_swift_refs2 %>% 
  ungroup() %>% 
  filter(is.na(chi) & 
           !is.na(ref_rec_date)) %>% 
  select(chi, upi, sub_source, hb_name, dataset_type) %>% 
  distinct() %>% 
  group_by(chi, upi, hb_name, dataset_type) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)



x=df_glob_swift_refs2 %>% 
  ungroup() %>% 
  filter(is.na(chi) #& 
           #!is.na(ref_rec_date)
         ) %>% 
  select(chi, upi, sub_source, hb_name, dataset_type, ref_rec_date) %>% 
  distinct() %>% 
  group_by(chi, upi, hb_name, dataset_type) %>% 
  inner_join(., y, by = c("chi", "upi", "hb_name", "dataset_type")) %>% 
  group_by(chi, upi, sub_source, hb_name, dataset_type) %>% 
  summarise(num = n())
  



  
  
  