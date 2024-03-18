##################################################.
### Add new and return appointment information ###.
##################################################.

#author: JBS
#date: 03/01/2024

## Figure out new/return appts from our own data

#important considerations

#new treatment start is effectively when a patient attended their 1st treat app

#all other apps (independent of their purpose or attendance status) are pre-treatment


add_new_return_apps <- function(df){
  
  df_apps <- df %>% 
    #filter(ucpn == "101024013482Z") |> 
    #ungroup() |> 
    #head(1000) |> 
    mutate(treat_app_date = case_when(
      str_detect(!!sym(rtt_eval_o) ,'seen') &
      !!sym(app_purpose_o) %in% c(2,3,5) & 
      !!sym(att_status_o) == 1 ~ !!sym(app_date_o))) %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(!!first_treat_app_o := case_when(
      is.na(treat_app_date) ~ NA_Date_,
      TRUE ~ min(treat_app_date, na.rm = TRUE))) %>% 
    fill(!!first_treat_app_o, .direction="downup") |> 
    ungroup() %>% 
    mutate(!!new_or_return_app_o := case_when(is.na(!!sym(app_date_o))|is.na(!!sym(first_treat_app_o)) ~ NA,
                                         !!sym(app_date_o)==!!sym(first_treat_app_o) ~ 'new - treatment start',
                                         !!sym(app_date_o)<!!sym(first_treat_app_o) &
                                           !is.na(!!sym(app_purpose_o)) ~ 'new - pre treatment',
                                         !!sym(app_date_o)>!!sym(first_treat_app_o) ~ 'return'),
           ) %>% 
    select(-c(treat_app_date))
  
  message('New and return apps added\n')
  
  return(df_apps)
  
}
