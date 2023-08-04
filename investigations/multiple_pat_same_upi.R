#Code to figure out if we have multiple patients with the same upi

#calculates number of upis tht correspond to more than one chi

chis_per_upi = df_glob_swift_refs %>% 
  select(hb_name, dataset_type,upi,chi) %>% 
  distinct() %>% 
  filter(!is.na(upi)) %>% 
  group_by(hb_name, dataset_type,upi) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(n>1)

upi_and_chi=df_glob_swift_refs %>% select(hb_name, dataset_type,upi,chi)


#quantify above calculation by board
mult_chis_upis_per_board = chis_per_upi %>% 
  group_by(hb_name,dataset_type) %>% 
  summarise(n_upi_with_mult_chi=n()) %>% 
  ungroup()

#Conclusion: there seems to be multiple patients with the same UPI


#Making document for Dougie with UPIs with mult chis, including the chis 
chis_per_upi_with_chis = chis_per_upi %>% 
  group_by(hb_name,dataset_type) %>% 
  inner_join(upi_and_chi, by=c('upi','hb_name', 'dataset_type'))%>% 
  ungroup()

write_csv(chis_per_upi_with_chis, '../../../output/investigations/multi_chis_per_upi.csv')

chis_per_upi_with_chis_list=chis_per_upi_with_chis %>% 
  group_by(hb_name) %>% 
  group_split()

for(df in chis_per_upi_with_chis_list){
  df=df %>% distinct()
  fname=unique(df$hb_name)
  write_csv(df,paste0('../../../output/investigations/multiple_chi_per_upi/mult_chi_per_upi_',fname,'.csv'))
}

#how many rows with a upi that has multiple chi exist, in which the chi is NA
q=df_glob_swift_refs %>% 
  filter(upi %in% chis_per_upi$upi & is.na(chi)) %>% 
  group_by(hb_name, dataset_type,upi,chi,ucpn) %>% 
  summarise(n=n())



