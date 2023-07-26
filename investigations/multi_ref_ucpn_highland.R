conflicts_prefer(dplyr::arrange)

multi_ref_per_pathway <- df_swift_clean %>% 
  select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) %>% 
  distinct() %>% 
  group_by(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
  filter(!is.na(!!sym(ref_rec_date_o))) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) 

multi_ucpn_highland <- multi_ref_per_pathway %>% 
  filter(!!sym(hb_name_o) == 'NHS Highland') %>% 
  pull(sym(ucpn_o))

multi_ref_ucpn_highland <- df_swift_clean %>% 
  filter(!is.na(!!sym(ref_rec_date_o))) %>%
  filter(!!sym(ucpn_o) %in% multi_ucpn_highland) %>% #filter out list of ucpns 
  group_by(!!sym(file_id_o), !!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o),
           !!sym(chi_o), !!sym(ref_rec_date_o)) %>% 
  summarise(n_ref_dates = n()) %>% #count referral dates
  ungroup() %>% 
  group_by(!!sym(dataset_type_o), !!sym(ucpn_o)) %>% 
  mutate(total=n()) %>% #total number of referrals per ucpn
  filter(total>1) %>% 
  select(-c(hb_name, n_ref_dates)) %>% 
  arrange(dataset_type, ucpn_o)


write_csv(multi_ref_ucpn_highland, paste0("../../../output/multi_ref_ucpn_highland.csv"))

