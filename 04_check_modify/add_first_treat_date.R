############################################.
### Add first treatment appointment date ###.
############################################.

#author: Luke Taylor
#date: 24/04/2026

#Update from previous script written by JBS in 2024.
#This script if far less time/memory intensive
#Calculates first_treat_app from all appts for each pathway


add_first_treat_date <- function(df){
  
  first_treat_date <- df_glob_swift_completed_rtt %>%
    filter(!!sym(app_purpose_o) %in% c(2, 3, 5),
           !!sym(att_status_o) == 1) %>%
    group_by(across(c(dataset_type, hb_name, patient_id, ucpn))) %>%
    summarise(!!first_treat_app_o := min(!!sym(app_date_o), na.rm = TRUE),
              .groups = "drop") |> ungroup()
  
  first_treat_date_complete <- df_glob_swift_completed_rtt %>%
    left_join(first_treat_date, by = c("hb_name", "dataset_type", "ucpn", "patient_id")) |>
    group_by(!!!syms(data_keys)) |>
    fill(!!first_treat_app_o, .direction="downup") |> ungroup()
  
  message('First treatment appt date added\n')
  
  return(first_treat_date_complete)

}

