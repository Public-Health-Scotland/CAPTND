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
    mutate(min_treat_app =  min(treat_app_date, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(new_or_return_app = case_when(is.na(!!sym(app_date_o))|is.na(min_treat_app) ~ NA,
                                         !!sym(app_date_o)==min_treat_app ~ 'new',
                                         !!sym(app_date_o)>min_treat_app ~ 'return')) %>% 
    select(-c(min_treat_app,treat_app_date))
  
  
}