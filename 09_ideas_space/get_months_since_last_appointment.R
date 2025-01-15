
########################################################
### Summarise months since last attended appointment ###
########################################################

# Author: CJS 
# Date: 2024-01-15

get_months_since_last_appointment <- function(df){
  
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  # filter out those that have never attended an appointment and those that have been discharged
  # df_test <- data.frame(
  #   hb_name = c(1, 1, 1, 2, 2, 3),
  #   dataset_type = c(1, 1, 1, 2, 2, 2),
  #   ucpn = c(12, 12, 12, 20, 20, 3),
  #   patient_id = c(100, 100, 100, 111, 111, 3),
  #   app_date = c("2024-05-06", "2024-06-10", "2024-7-20", "2024-08-17", NA_Date_, "2025-01-01"),
  #   dis_date = c(NA_Date_, NA_Date_, NA_Date_, NA_Date_, NA_Date_, "2025-01-11")
  #   
  # )
  
  df_work <- df |> 
    group_by(!!!syms(c(hb_name_o, dataset_type_o, ucpn_o, patient_id_o))) |> 
    arrange(!!!syms(c(ucpn_o, app_date_o))) |> 
    filter(any(is.na(!!sym(case_closed_date_o)))) |> # must not have been discharged
    ungroup() |> 
    filter(!is.na(!!sym(app_date_o)) & !!sym(att_status_o) == 1 ) |>  # must have an attended app date 
    group_by(!!!syms(c(hb_name_o, dataset_type_o, ucpn_o, patient_id_o))) |> 
    filter(!!sym(app_date_o) == last(!!sym(app_date_o))) |> 
    mutate(time_since_last_app )
  
  
}
