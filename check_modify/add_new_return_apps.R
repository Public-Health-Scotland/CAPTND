##################################################.
### Add new and return appointment information ###.
##################################################.

#author: JBS
#date: 03/01/2024

## Figure out new/return appts from our own data

#only considers people who have been seen for treatment
#patients that were seen for assessment or who haven't attended their treatment
#appointments are considered NA

add_new_return_apps <- function(df){
  
  df_apps <- df %>% 
    mutate(treat_app_date = case_when(str_detect(!!sym(rtt_eval_o) ,'seen') &
                                        !!sym(app_purpose_o) %in% c(2,3,5) & 
                                        !!sym(att_status_o) == 1 ~ !!sym(app_date_o))) %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(!!first_treat_app_o :=  min(treat_app_date, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(!!new_or_return_app_o := case_when(is.na(!!sym(app_date_o))|is.na(!!sym(first_treat_app_o)) ~ NA,
                                         !!sym(app_date_o)==!!sym(first_treat_app_o) ~ 'new - treatment start',
                                         !!sym(app_date_o)<!!sym(first_treat_app_o) &
                                           !!sym(app_purpose_o) %in% c(2,3,5) ~ 'new - pre treatment',
                                         !!sym(app_date_o)>!!sym(first_treat_app_o) ~ 'return'),
           ) %>% 
    select(-c(treat_app_date))
  
  message('New and return apps added\n')
  
  return(df_apps)
  
  
}