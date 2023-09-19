
# Function to remove records with multiple referral dates per pathway

source('reporting/report_multiple_ref_per_journey.R')


remove_multi_ref_pathways <- function(df, stage_name){

  # check ref dates per key
  multi_ref_per_pathway <- df %>% 
    select(sym(ref_rec_date_o), sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) %>% 
    distinct() %>% 
    group_by(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
    filter(!is.na(!!sym(ref_rec_date_o))) %>% 
    summarise(n = n(), .groups = 'drop') %>% 
    filter(n > 1) %>% 
    select(sym(patient_id_o), sym(ucpn_o), sym(hb_name_o), sym(dataset_type_o)) 
  
  unique_ref_per_pathway <- anti_join(df, multi_ref_per_pathway,by = join_by(dataset_type, hb_name, ucpn, patient_id))
  
  write_csv(multi_ref_per_pathway %>% mutate(issue='multi ref on pathway'),
            paste0('../../../output/removed/details_removed/',
            stage_name,
            '_details_removed_multi_ref_path_',
            DATA_FOLDER_LATEST,
            '.csv'))
  
  report_mult_ref_journey(df,multi_ref_per_pathway, stage_name)
  
  return(unique_ref_per_pathway)

}