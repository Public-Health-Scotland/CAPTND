#############################################
######  Fix multiple discharge dates  #######    
#############################################

# Author: Luke Taylor
# Date: 2025-05-26

# Issue: Some HBs are submitting more than one discharge date for each pathway

# Solution: This function keeps the most recent discharge date that is valid for
# each patient pathway. A valid case closed date is any case closed date that is
# after the most recent attended appointment. Case closed dates before an 
# attended appointment will be ignored.


check_multi_discharge_dates <- function(df){
  
  df_multi_dis_date <- df |>
    group_by(!!!syms(data_keys)) |>
    arrange(ucpn) |>
    lazy_dt() |>
    #identify all attended appts for each patient pathway
    mutate(att_app_dates = case_when(att_status == 1 ~ app_date,
                                     TRUE ~ NA_Date_)) |>
    #identify the most recent attended appt for each patient pathway
    mutate(max_att_app_date = max(att_app_dates, na.rm = TRUE)) |>
    #identify the last reported case closed date for each patient pathway
    mutate(last_reported_case_closed_date = max(case_closed_date, na.rm = TRUE)) |>
    #keep valid case closed dates
    mutate(case_closed_opti = case_when(is.na(last_reported_case_closed_date) ~ NA_Date_, #no case closed date
                                        is.na(max_att_app_date) & !is.na(last_reported_case_closed_date) ~ last_reported_case_closed_date, #no appt, keep case closed date
                                        last_reported_case_closed_date >= max_att_app_date ~ last_reported_case_closed_date, #all appts before case closed date, keep case closed date
                                        last_reported_case_closed_date < max_att_app_date ~ NA_Date_, #case closed date before most recent appt, not valid
                                        TRUE ~ NA_Date_)) |>
    ungroup() |> 
    as.data.frame() |>
    select(-att_app_dates, -max_att_app_date, -last_reported_case_closed_date)
  
  
  # df_multi_dis_date <- df |>
  #   group_by(!!!syms(data_keys)) |> 
  #   arrange(ucpn) |>
  #   lazy_dt() |>
  #   #identify most recent appointment date for each patient
  #   mutate(max_app_date = max(!!sym(app_date_o), na.rm = TRUE)) |>
  #   #mutate(max_app_date = if_else(!is.na(!!sym(app_date_o)), max(!!sym(app_date_o), na.rm = TRUE), NA)) |>
  #   #identify valid case closed dates
  #   mutate(valid_case_closed_dates = case_when(is.na(case_closed_date) ~ NA_Date_, #no case closed date
  #                                              is.na(max_app_date) & !is.na(case_closed_date) ~ case_closed_date, #no appt, keep case closed date
  #                                              case_closed_date >= max_app_date ~ case_closed_date, #all appts before case closed date, keep case closed date
  #                                              case_closed_date < max_app_date ~ NA_Date_, #case closed date before most recent appt, not valid
  #                                              TRUE ~ NA_Date_)) |>
  #   #keep earliest valid case closed date
  #   mutate(case_closed_opti = min(valid_case_closed_dates, na.rm = TRUE)) |>
  #   ungroup() |> 
  #   as.data.frame() |>
  #   select(-max_app_date, -valid_case_closed_dates)
  # 
  # message('Valid discharge dates identified and new optimised column created\n')
  # 
  # return(df_multi_dis_date)
  
}

