
# library(arrow)
# library(dplyr)
# library(readr)
# library(lubridate)


report_multiple_apps <- function(df_glob_swift_completed, date_extract){
  
  tryCatch(
    {
      date_extract <- as.character(ymd(date_extract))
    },
    warning = function(e){
      stop(paste('Enter date in YMD format.',date_extract,'os not valid.'))
    },
    error = function(e) { 
      stop(paste('Enter date in YMD format.',date_extract,'os not valid.'))
    })
  
  apps_df <- df_glob_swift_completed %>% 
    mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>%  
    select(all_of(c(data_keys,vec_app_cols)),!!submission_date_o) %>% 
    distinct() %>% 
    filter(!is.na(!!sym(app_date_o)))
  
  apps=c(data_keys,app_date_o,submission_date_o)
  
  apps_df_multi=apps_df %>% 
    group_by(across(all_of(apps))) %>% 
    summarise(n_apps_same_day=n(), .groups = 'drop') %>% 
    filter(n_apps_same_day>1) 
  
  
  apps_df_multi_detail=apps_df_multi %>% 
    inner_join(apps_df, by=c(apps),relationship='many-to-many')
  
  
  apps_by_board=apps_df_multi %>% 
    group_by(!!sym(dataset_type_o),!!sym(hb_name_o)) %>% 
    summarise(encounters_with_multi_apps=n(), .groups = 'drop')
  
  write_csv(apps_df_multi_detail, paste0('../../../problems/multi_apps_detailed_',date_extract,'.csv')) 
  write_csv(apps_by_board, paste0('../../../problems/multi_apps_boards_',date_extract,'.csv')) 
  
}

