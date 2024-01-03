
## Quantify potential temp CHI


df_investig <- df %>% 
  select(all_of(data_keys)) %>% 
  distinct() %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(ucpn_o)) %>% 
  summarise(n=n(), .groups = 'drop') %>% 
  filter(n>1)

#28 rows