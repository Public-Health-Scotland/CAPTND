#Code to figure out if we have multiple patients with the same upi

#calculates number of upis tht correspond to more than one chi
y = df_glob_swift_refs %>% 
  select(hb_name, dataset_type,upi,chi) %>% 
  distinct() %>% 
  filter(!is.na(upi)) %>% 
  group_by(hb_name, dataset_type,upi) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(n>1)

#quantify above calculation by board
z = y %>% group_by(hb_name,dataset_type) %>% 
  summarise(n_upi_with_mult_chi=n())

#Conclusion: there seems to be multiple patients with the same UPI