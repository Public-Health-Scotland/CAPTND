###########################.
## Calculates open cases ##.
###########################.

#author: JBS
#last updated: 22/01/24

# Open case is defined by a patient that has been seen for treatment
# and has not been discharged yet
# Therefore, we only considered patients seen as open cases

source('calculations/save_data_board.R')

calculate_open_cases <- function(df_glob_swift_completed_rtt, most_recent_month_in_data) {
  
  #check
  df_open <- df_glob_swift_completed_rtt %>% 
    group_by(across(all_of(data_keys))) %>% 
    filter(!!sym(rtt_eval_o) %in% c("seen - active",
                                   "seen - online - active")) %>% 
    mutate(weeks_since_last_app= case_when(
      any(!is.na(!!sym(app_date_o))) ~ as.numeric(ceiling(difftime(
      most_recent_month_in_data, max(!!sym(app_date_o), na.rm = TRUE), units = "weeks"))),
      TRUE ~ NA),
      weeks_since_code_sent= case_when(
        any(!is.na(!!sym(act_code_sent_date_o))) ~ as.numeric(ceiling(difftime(
          most_recent_month_in_data, max(!!sym(act_code_sent_date_o), na.rm = TRUE), units = "weeks"))),
        TRUE ~ NA),
      max_app_date = max(!!sym(app_date_o), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o, max_app_date, !!act_code_sent_date_o,
           weeks_since_last_app, weeks_since_code_sent, sub_source_eval, !!rtt_eval_o) %>% 
    distinct()
  
  open_cases_sub_source=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),sub_source_eval) %>% 
    summarise(n_open_cases=n(), .groups = 'drop') 
  
  
  x=open_cases_sub_source %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases', save_data_board, open_cases_dir_by_board)
  
  y=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases_details', save_data_board, open_cases_dir_by_board)
  
  write_csv_arrow(open_cases_sub_source, paste0(open_cases_dir,'/openCases_subSource.csv'))
  
  message(paste0('Your output files are in ',open_cases_dir))
  

}




