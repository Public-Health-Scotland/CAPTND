library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)



calculate_patients_waiting <- function(df_glob_swift_completed_rtt, extractDate) {
  
  extractDate=ymd(230921)
  
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
             all(is.na(!!sym(app_date_o)))&
             all(is.na(!!sym(case_closed_date_o)))) %>%
    mutate(patient_status='waiting first appointment',
           waitingTime=time_length(extractDate-!!sym(ref_rec_date_opti_o),"week") %>% round(.,0),
           lastUpdate=max(!!sym(header_date_o))) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,waitingTime,sub_source_eval,lastUpdate) %>% 
    distinct()
  
  df_pat_waitingTime_seen <-
    df_glob_swift_completed_rtt %>% 
    filter(!!sym(ref_acc_o)==1 &
             !is.na(!!sym(app_date_o))&
             !!sym(app_purpose_o) %in% c(2,3) &
             !!sym(att_status_o)==1 ) %>% 
    group_by(across(all_of(data_keys))) %>%
    filter(!!sym(app_date_o)==min(!!sym(app_date_o))) %>% 
    mutate(patient_status='started treatment',
           waitingTime=time_length(!!sym(app_date_o)-!!sym(ref_rec_date_opti_o),"week") %>% round(.,0),
           app_month=floor_date(!!sym(app_date_o), 'month')) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,!!app_date_o,waitingTime,app_month,sub_source_eval) %>% 
    distinct()
  
  
  df_n_patWaiting=df_pat_waitingTime %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),sub_source_eval) %>% 
    summarise(n_patients_waiting=n(), .groups = 'drop') 
  
  df_n_pat_waitingTime_seen_by_week=df_pat_waitingTime_seen %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),app_month,waitingTime) %>% 
    summarise(n=n(), .groups = 'drop')
  
  df_n_pat_seen_1st_treat_app=df_n_pat_waitingTime_seen_by_week %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),app_month) %>% 
    summarise(n=n(), .groups = 'drop')
  
  x=df_n_patWaiting %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'n_patients_waiting', save_data_board)
  
  y=df_pat_waitingTime %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'waiting_times_not_seen', save_data_board)
  
  z=df_n_pat_waitingTime_seen_by_week %>% 
    filter(app_month > (max(app_month)%m-% months(3))) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'patients_seen_waiting_time', save_data_board)
    
  
  write_csv_arrow(df_n_patWaiting, '../../../output/calculations/nPatients_waiting_subSource.csv')
  write_csv_arrow(df_pat_waitingTime, '../../../output/calculations/patients_waitingTimes_notSeen_subSource.csv')
  write_csv_arrow(df_pat_waitingTime_seen, '../../../output/calculations/patients_waitingTimes_seen_subSource.csv')
  write_csv_arrow(df_n_pat_seen_1st_treat_app, '../../../output/calculations/patients_seen_1st_treat_app.csv')
  
  
}

