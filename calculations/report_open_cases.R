library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)



calculate_open_cases <- function(df_glob_swift_completed_rtt) {
  
  
  save_data_board <-function(df,issue){
    
    df_name_dataset_type <- df %>% 
      select(!!hb_name_o,!!dataset_type_o) %>% 
      distinct() %>% 
      unite(full_name,c(!!hb_name_o,!!dataset_type_o),sep = "_") %>% 
      pull(full_name) 
    
    write_csv_arrow(df, paste('../../../output/calculations/openCases/byBoard/',
                              df_name_dataset_type,
                              '_',
                              issue,
                              '.csv'))
    
  }
  
  df_open <- df_glob_swift_completed_rtt %>% 
    group_by(across(all_of(data_keys))) %>% 
    filter(any(!is.na(!!sym(app_date_o)))&
             all(is.na(!!sym(case_closed_date_o)))) %>%
    mutate(time_since_last_app=today()-max(!!sym(app_date_o), na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(all_of(data_keys),!!ref_rec_date_opti_o,time_since_last_app,sub_source_eval) %>% 
    distinct()
  
  open_cases_sub_source=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o),sub_source_eval) %>% 
    summarise(n_open_cases=n(), .groups = 'drop') 
  
  
  x=open_cases_sub_source %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases', save_data_board)
  
  y=df_open %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'open_cases_details', save_data_board)
  
  write_csv_arrow(open_cases_sub_source, '../../../output/calculations/openCases_subSource.csv')
  

}




