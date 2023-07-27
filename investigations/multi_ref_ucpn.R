conflicts_prefer(dplyr::arrange)

#multi_ref_per_pathway <- df_swift_clean %>% 
 # select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) %>% 
  #distinct() %>% 
  # group_by(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
  # filter(!is.na(!!sym(ref_rec_date_o))) %>% 
  # summarise(n = n()) %>% 
  # filter(n > 1) 

#multi_ucpn_highland <- multi_ref_per_pathway %>% 
#  filter(!!sym(hb_name_o) == 'NHS Highland') %>% 
#  pull(sym(ucpn_o))

multi_ref_ucpn_list <- df_swift_clean %>% 
  select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) %>% 
  distinct() %>% 
  filter(!is.na(!!sym(ref_rec_date_o))) %>%
  #filter(!!sym(ucpn_o) %in% multi_ucpn_highland) %>% #filter out list of ucpns 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o),!!sym(patient_id_o), !!sym(ucpn_o)) %>% 
  summarise(n_ref_dates = n()) %>% #count referral dates
  ungroup() %>% 
  filter(n_ref_dates>1) %>% 
  group_by(!!sym(hb_name_o)) %>% 
  group_split()


for (df in multi_ref_ucpn_list){
  nameoffile = unique(df$hb_name)
 
  write_csv(df, paste0("../../../output/multi_ref_",nameoffile,".csv"))
   
}


