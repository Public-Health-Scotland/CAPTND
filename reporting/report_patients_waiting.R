library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)



calculate_patients_waiting <- function(df_glob_swift_completed_rtt) {
  
  
  save_data_board <-function(df,issue){
    
    df_name_dataset_type <- df %>% 
      select(!!hb_name_o,!!dataset_type_o) %>% 
      distinct() %>% 
      unite(full_name,c(!!hb_name_o,!!dataset_type_o),sep = "_") %>% 
      pull(full_name) 
    
    write_csv_arrow(df, paste('../../../output/calculations/patientsWaiting/byBoard/',
                              df_name_dataset_type,
                              '_',
                              issue,
                              '.csv'))
    
  }
  
  df_pat_waitingTime <- df_glob_swift_completed_rtt %>% 
    group_by(across(all_of(data_keys))) %>% 
    filter(any(!!sym(ref_acc_o)==1)&
             all(is.na(!!sym(app_date_o)))) %>%
    mutate(waitingTime=today()-!!sym(ref_rec_date_opti_o)) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,waitingTime,sub_source_eval) %>% 
    distinct()
  
  df_n_patWaiting=df_pat_waitingTime %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),sub_source_eval) %>% 
    summarise(n_patients_waiting=n(), .groups = 'drop') 
  
  
  x=df_n_patWaiting %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'n_patients_waiting', save_data_board)
  
  y=df_pat_waitingTime %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'waiting_times_not_seen', save_data_board)
  
  write_csv_arrow(df_n_patWaiting, '../../../output/calculations/nPatients_waiting_subSource.csv')
  write_csv_arrow(df_pat_waitingTime, '../../../output/calculations/patients_waitingTimes_notSeen_subSource.csv')
  
  
}

