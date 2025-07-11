###################################.
### Clean df for WL calculation ###.
###################################.

#Author: LukeTaylor
#Date: 07/07/2025

## Remove appointment records before ref_rec_date_opti and recalulate first treatment appt
## Remove case closed dates the pre-date referral recieved dates 

source("04_check_modify/add_new_return_apps.R")

clean_df_for_wl <- function(df){
  
  df_apps <- df %>% 
    group_by(!!!syms(data_keys)) |>
    mutate(has_impossible_app_date = fcase(any(app_date < ref_rec_date_opti), TRUE,
                                           default = FALSE)) |>
    ungroup() |>
    filter(has_impossible_app_date == TRUE) |>
    filter(app_date >= ref_rec_date_opti | is.na(app_date)) |>
    add_new_return_apps() |>
    rename(adj_first_treat_app = first_treat_app) |>
    select(!!!syms(data_keys), adj_first_treat_app)
  
  
  df_clean <- df |>
    filter(case_closed_opti >= ref_rec_date_opti | is.na(case_closed_opti),
           app_date >= ref_rec_date_opti | is.na(app_date)) |>
    full_join(df_apps, by = c('dataset_type', 'hb_name', 'patient_id', 'ucpn')) |>
    mutate(first_treat_app = case_when(!is.na(adj_first_treat_app) ~ adj_first_treat_app,
                                       TRUE ~ first_treat_app)) |>
    distinct() |>
    select(-adj_first_treat_app)
  
  message('Dataframe cleaned for WL calculation\n')
  
  return(df_clean)
  
}
