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
source('reporting/report_negative_waits_seen.R')



calculate_patients_seen <- function(df_glob_swift_completed_rtt) {
  
  df_pat_waiting_time_seen <- df_glob_swift_completed_rtt %>%  
      filter(str_detect(!!sym(rtt_eval_o), 'seen') &
               str_detect(!!sym(rtt_eval_o), 'online', negate = TRUE)&
               !!sym(att_status_o) == 1 &
               !!sym(app_purpose_o) %in% c(2,3,5))  %>%  
    group_by(across(all_of(data_keys))) %>%
    mutate(first_treat_app=min(!!sym(app_date_o))) %>% 
    filter(!!sym(app_date_o)==first_treat_app) %>% 
    mutate(patient_status='started treatment',
           waiting_time=as.numeric(ceiling(difftime(first_treat_app, !!sym(ref_rec_date_opti_o), units = "weeks")))) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,first_treat_app,waiting_time,!!app_month_o,sub_source_eval,!!rtt_eval_o) %>% 
    distinct()
  
  df_n_pat_waiting_time_seen_by_week=df_pat_waiting_time_seen %>% 
    filter(str_detect(!!sym(rtt_eval_o), 'online', negate = TRUE)) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(app_month_o),waiting_time) %>% 
    summarise(n=n(), .groups = 'drop')
  
  df_n_pat_seen_1st_treat_app=df_n_pat_waiting_time_seen_by_week %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(app_month_o)) %>% 
    summarise(n=n(), .groups = 'drop')
  
  z=df_n_pat_waiting_time_seen_by_week %>% 
    #filter(app_month > (max(app_month)%m-% months(3))) %>% #filter exists here to compare with aggregate
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'patients_seen_waiting_time', save_data_board, patients_seen_dir_by_board)
  
  #negative waiting times
  df_negative <- df_pat_waiting_time_seen %>% 
    filter(waiting_time<0) 
  
  w=df_negative %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'negative_waiting_time', save_data_board, patients_seen_dir_by_board)
  
  write_csv_arrow(df_n_pat_waiting_time_seen_by_week, paste0(patients_seen_dir,'/patients_waitingTimes_seen_subSource.csv'))
  write_csv_arrow(df_n_pat_seen_1st_treat_app, paste0(patients_seen_dir,'/patients_seen_1st_treat_app.csv'))
  write_csv_arrow(df_negative, paste0(patients_seen_dir,'/patients_negative_waitingTimes_seen.csv'))
  
  
  report_negative_waits(df_negative)
  
  
  message(paste('Your files are in',patients_seen_dir))
  
}


