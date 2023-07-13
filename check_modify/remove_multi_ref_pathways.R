
# Function to remove records with multiple referral dates per pathway




remove_multi_ref_pathways <- function(df){

  # check ref dates per key
  multi_ref_per_pathway <- df %>% 
    select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) %>% 
    distinct() %>% 
    group_by(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
    filter(!is.na(!!sym(ref_rec_date_o))) %>% 
    summarise(n = n()) %>% 
    filter(n > 1) %>% 
    ungroup() %>% 
    select(sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) 
  
  unique_ref_per_pathway <- anti_join(df, multi_ref_per_pathway)
  
  return(unique_ref_per_pathway)

}