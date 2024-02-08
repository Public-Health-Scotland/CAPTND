#author: JBS
#date: Jan/2024


## Quantify potential temp CHI


df_investig <- df %>% 
  select(!!hb_name_o, !!dataset_type_o, !!ucpn_o, !!chi_o, !!upi_o) %>% 
  distinct() %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(ucpn_o)) %>% 
  summarise(n=n(), .groups = 'drop') %>% 
  filter(n>1) %>% 
  inner_join(df, by=c(hb_name_o, dataset_type_o, ucpn_o)) %>% 
  select(c(!!hb_name_o, !!dataset_type_o, !!ucpn_o, !!chi_o, !!upi_o, !!patient_id_o, n)) %>% 
  distinct()
  


df_investig_chi_na <- df_investig %>% 
  filter(is.na(chi)) 

# Turns out this is zero
