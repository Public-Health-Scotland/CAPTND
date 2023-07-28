
# check ref dates per key

multi_ref_per_pathway <- df_swift_clean %>% 
  select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) %>% 
  distinct() %>% 
  group_by(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
  filter(!is.na(!!sym(ref_rec_date_o))) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) 

multi_ref_per_pathway_hb <- multi_ref_per_pathway %>% 
  ungroup() %>% 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) %>% 
  summarise(n = n())


library(rio)
export(test_hb, file = "../../../output/number_multi_ref_per_pathway_hb_July_2023.csv")


multi_refs_to_remove <- multi_ref_per_pathway %>% 
  ungroup() %>% 
  select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) 

multi_refs_to_remove <- anti_join(mul)




