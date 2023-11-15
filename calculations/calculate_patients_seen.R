library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)
library(purrr)
library(tidyr)
library(conflicted)
conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')

source('config/new_colnames.R')
source('calculations/save_data_board.R')




calculate_patients_seen <- function(df_glob_swift_completed_rtt, extractDate) {
  
  df_pat_waitingTime_seen <-
    df_glob_swift_completed_rtt %>% 
    filter(!!sym(ref_acc_o)==1 &
             !is.na(!!sym(app_date_o))&
             !!sym(app_purpose_o) %in% c(2,3) &
             !!sym(att_status_o)==1 ) %>% 
    group_by(across(all_of(data_keys))) %>%
    filter(!!sym(app_date_o)==min(!!sym(app_date_o))) %>% 
    mutate(patient_status='started treatment',
           waitingTime=ceiling(difftime(!!sym(app_date_o), !!sym(ref_rec_date_opti_o), units = "weeks")),
           app_month=floor_date(!!sym(app_date_o), 'month')) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,!!app_date_o,waitingTime,app_month,sub_source_eval) %>% 
    distinct()
  
  df_n_pat_waitingTime_seen_by_week=df_pat_waitingTime_seen %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),app_month,waitingTime) %>% 
    summarise(n=n(), .groups = 'drop')
  
  df_n_pat_seen_1st_treat_app=df_n_pat_waitingTime_seen_by_week %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),app_month) %>% 
    summarise(n=n(), .groups = 'drop')
  
  z=df_n_pat_waitingTime_seen_by_week %>% 
    filter(app_month > (max(app_month)%m-% months(3))) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'patients_seen_waiting_time', save_data_board, patients_seen_dir_by_board)
  
  write_csv_arrow(df_n_pat_waitingTime_seen_by_week, paste0(patients_seen_dir,'/patients_waitingTimes_seen_subSource.csv'))
  write_csv_arrow(df_n_pat_seen_1st_treat_app, paste0(patients_seen_dir,'/patients_seen_1st_treat_app.csv'))
  
  
  message(paste('Your files are in',patients_seen_dir))
  
}


