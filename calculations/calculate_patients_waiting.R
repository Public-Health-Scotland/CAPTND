# library(dplyr)
# library(lubridate)
# library(arrow)
# library(phsmethods)
# library(purrr)
# library(tidyr)
# library(conflicted)
# conflict_prefer('filter','dplyr')
# conflict_prefer('mutate','dplyr')

source('calculations/save_data_board.R')


calculate_patients_waiting <- function(df_glob_swift_completed_rtt, most_recent_month_in_data) {
  
  df_pat_waitingTime <- df_glob_swift_completed_rtt %>% 
    group_by(across(all_of(data_keys))) %>% 
    filter(str_detect(!!sym(rtt_eval_o), 'waiting')) %>%
    mutate(patient_status='waiting treatment',
           waitingTime=as.numeric(ceiling(difftime(most_recent_month_in_data, !!sym(ref_rec_date_opti_o), units = "weeks"))),
           lastUpdate=max(!!sym(header_date_o), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,waitingTime,sub_source_eval,lastUpdate) %>% 
    distinct()
  
  
  df_n_patWaiting=df_pat_waitingTime %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),sub_source_eval) %>% 
    summarise(n_patients_waiting=n(), .groups = 'drop') 
  
  
  x=df_n_patWaiting %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'n_patients_waiting', save_data_board, patients_waiting_dir_by_board)
  
  y=df_pat_waitingTime %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'waiting_times_not_seen', save_data_board, patients_waiting_dir_by_board)
  
  write_csv_arrow(df_n_patWaiting, paste0(patients_waiting_dir,'/nPatients_waiting_subSource.csv'))
  write_csv_arrow(df_pat_waitingTime, paste0(patients_waiting_dir,'/patients_waitingTimes_notSeen_subSource.csv'))
 
  
  message(paste('Your files are in',patients_waiting_dir))
  
}

