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
      filter(!!sym(new_or_return_app_o) == 'new - treatment start' |
               !is.na(!!sym(act_code_sent_date_o)))  %>%  
    group_by(across(all_of(data_keys))) %>%
    mutate(clock_stop=case_when(is.na(!!sym(new_or_return_app_o)) ~ !!sym(act_code_sent_date_o),
                                is.na(!!sym(act_code_sent_date_o)) ~ !!sym(first_treat_app_o),
                                !!sym(first_treat_app_o) <= !!sym(act_code_sent_date_o) ~ !!sym(first_treat_app_o),
                                !!sym(first_treat_app_o) > !!sym(act_code_sent_date_o) ~ !!sym(act_code_sent_date_o)),
           clock_stop_month=floor_date(clock_stop, 'month')) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,clock_stop,clock_stop_month,sub_source_eval,!!rtt_eval_o) %>% 
    distinct() %>% 
    mutate(waiting_time=as.numeric(ceiling(difftime(clock_stop, !!sym(ref_rec_date_opti_o), units = "weeks")))) %>% 
    rename(app_month = clock_stop_month,
           first_treat_app = clock_stop)
    
  
  df_n_pat_waiting_time_seen_by_week=df_pat_waiting_time_seen %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),app_month,waiting_time) %>% 
    summarise(n=n(), .groups = 'drop')
  
  
  z=df_n_pat_waiting_time_seen_by_week %>% 
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
  write_csv_arrow(df_negative, paste0(patients_seen_dir,'/patients_negative_waitingTimes_seen.csv'))
  
  report_negative_waits(df_negative)

  
  
  message(paste('Your files are in',patients_seen_dir))
  
}


