###########################.
## Calculates open cases ##.
###########################.

#author: JBS
#last updated: 22/01/24

# Open case is defined by a patient that has been seen for treatment
# and has not been discharged yet
# Therefore, we only considered patients seen as open cases

source('06_calculations/save_data_board.R')

calculate_open_cases <- function(df_glob_swift_completed_rtt, most_recent_month_in_data) {
  
  df_open <- df_glob_swift_completed_rtt %>% 
    group_by(across(all_of(data_keys))) %>% 
    filter(!!sym(rtt_eval_o) %in% c("seen - active",
                                   "seen - online - active",
                                   "waiting - after assessment",
                                   "rtt not possible - attended app but no purpose",
                                   "rtt not possible - patient had appt and ref is pending",
                                   "rtt not possible - app with no referral acc") &
            all(is.na(!!sym(case_closed_date_o)))) %>% 
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
    distinct() |> 
    save_as_parquet(path = paste0(comp_report_dir_patient_data, "/open_cases"))
  
  #calculating service demand and treatament caseload
  #treatment caseload is included in service demand
  open_cases_sub_source=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(rtt_eval_o)) %>% 
    summarise(n=n(), .groups = 'keep') %>% 
    mutate(demand_type = if_else(str_detect(!!sym(rtt_eval_o), 'seen'), 'treatment caseload', 'post assessment demand')) %>%
    ungroup() %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    mutate(post_assessment_demand=sum(n)) %>% 
    filter(demand_type == 'treatment caseload') %>% 
    mutate(n=sum(n)) %>% 
    ungroup() %>% 
    select(!!sym(hb_name_o),!!sym(dataset_type_o), treatment_caseload = n, post_assessment_demand) %>% 
    distinct() %>% 
    pivot_longer(c(treatment_caseload,post_assessment_demand), names_to = 'demand_type', values_to = 'n' )
  
  df_open_complete <- df_glob_swift_completed_rtt %>% 
    select(all_of(data_keys),!!ref_acc_last_reported_o, !!case_closed_date_o) %>% 
    distinct() %>% 
    filter(!!sym(ref_acc_last_reported_o) != 2 &
           is.na(!!sym(case_closed_date_o))) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    summarise(n=n(),
              .groups = 'drop') %>% 
    mutate(demand_type='total_service_demand') %>% 
    bind_rows(open_cases_sub_source)
    
  
  
  x=df_open_complete %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases', save_data_board, open_cases_dir_by_board)
  
  y=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases_details', save_data_board, open_cases_dir_by_board)
  
  write_csv_arrow(df_open_complete, paste0(open_cases_dir,'/openCases_subSource.csv'))
  
  message(paste0('Your output files are in ',open_cases_dir))
  

}




