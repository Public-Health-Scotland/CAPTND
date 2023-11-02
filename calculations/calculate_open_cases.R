library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)
library(conflicted)
source('calculations/save_data_board.R')
conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')
conflict_prefer('summarise', 'dplyr')


calculate_open_cases <- function(df_glob_swift_completed_rtt,extract_date) {
  
  df_open <- df_glob_swift_completed_rtt %>% 
    group_by(across(all_of(data_keys))) %>% 
    filter(any(!is.na(!!sym(app_date_o))) &
             all(is.na(!!sym(case_closed_date_o)))) %>% 
    mutate(weeks_since_last_app=ceiling(difftime(extract_date, max(!!sym(app_date_o), na.rm = TRUE), units = "weeks"))) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,weeks_since_last_app,sub_source_eval) %>% 
    distinct()
  
  open_cases_sub_source=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),sub_source_eval) %>% 
    summarise(n_open_cases=n(), .groups = 'drop') 
  
  
  x=open_cases_sub_source %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases', save_data_board, 'openCases')
  
  y=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases_details', save_data_board, 'openCases')
  
  write_csv_arrow(open_cases_sub_source, '../../../output/calculations/openCases_subSource.csv')
  
  message('Your output files are in ../../../output/calculations/openCases/')
  

}




