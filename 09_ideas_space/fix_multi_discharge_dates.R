#############################################
######  Fix multiple discharge dates  #######    
#############################################

# Author: Luke Taylor
# Date: 2025-05-26

# Issue: Some HBs are submitting more than one discharge date for each pathway

# Solution: This function aims to keep the earliest discharge date after the last 
# recorded appt date. If the most recent appt date is after the most recent discharge
# date then the discharge date should be set to NA.


multi_discharge_dates <- function(df){
  
  df_multi_dis_date <- df |>
    group_by(!!!syms(data_keys)) |> 
    arrange(ucpn) |>
    lazy_dt() |>
    #identify most recent appointment date for each patient
    mutate(max_app_date = max(!!sym(app_date_o), na.rm = TRUE)) |>
    #identify valid case closed dates
    mutate(valid_case_closed_dates = case_when(is.na(case_closed_date) ~ NA_Date_, #no case closed date
                                               is.na(max_app_date) & !is.na(case_closed_date) ~ case_closed_date, #no appt, keep case closed date
                                               case_closed_date >= max_app_date ~ case_closed_date, #all appts before case closed date, keep case closed date
                                               case_closed_date < max_app_date ~ NA_Date_, #case closed date before most recent appt, not valid
                                               TRUE ~ NA_Date_)) |>
    #keep earliest valid case closed date
    mutate(case_closed_opti = min(valid_case_closed_dates, na.rm = TRUE)) |>
    ungroup() |> 
    as.data.frame() |>
    select(-max_app_date, -valid_case_closed_dates)
  
  message('Valid discharge dates identified and new optimised column created\n')
  
  return(df_multi_dis_date)
  
}