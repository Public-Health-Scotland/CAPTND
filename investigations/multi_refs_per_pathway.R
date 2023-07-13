
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


# small scale example
df_test <- data.frame(
  ref_rec_date = c(1, 2, 3, 4),
  patient_id = c(12345, 678910, 678910, 678910), 
  ucpn = c(1, 2, 2, 3),
  hb_name = c("a", "a", "a", "a"),
  dataset_type = c("CAMHS", "PT", "CAMHS", "PT")
)

test2 <- df_test %>% 
  select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o)#,
         #sym(dataset_type_o)
  ) %>% 
  distinct() %>% 
  group_by(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(hb_name_o)#, 
           #!!sym(dataset_type_o)
  ) %>% 
  summarise(n = n()) %>% 
  filter(n > 2)


