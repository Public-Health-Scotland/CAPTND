


postcode_areas=read_csv('../../../data/postcode_areas.csv')

attend=df_glob_swift_completed %>% filter(!!sym(att_status_o)==8 & !is.na(!!sym(postcode_o))) %>% 
  select(all_of(data_keys),!!postcode_o) %>% 
  distinct() %>% 
  mutate(postcode_start=str_sub(postcode,1,2)) %>% 
  inner_join(postcode_areas)