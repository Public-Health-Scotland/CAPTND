
################################.
### Assess Cancellation Date ###
################################.

# Author: Charlie Smith
# Date: 2024-04-12


# assess_cancellation_date <- function(df){
# 
#   df_cancellation_date <- df |>
#     mutate(check_cancellation_date = case_when(
#       !!sym(att_status_o) %in% c("02", "03") & !is.na(!!sym(cancellation_date_o)) ~ "valid", # cancelled by clinic, by patient, or patient died AND has cancel date
#       !!sym(att_status_o) %in% c("02", "03") & is.na(!!sym(cancellation_date_o)) ~ "missing",
#       !!sym(att_status_o) %in% c("01", "05", "08", "09") & is.na(!!sym(cancellation_date_o)) ~ "missing but valid",
#       TRUE ~ "missing but valid"))
# 
#   return(df_cancellation_date)
#   
# }


assess_cancellation_date <- function(df){
  
  df_cancellation_date <- df |>
    mutate(check_cancellation_date = case_when(
      !!sym(att_status_o) %in% c("02", "03") & !is.na(!!sym(cancellation_date_o)) ~ "valid", # cancelled by clinic, by patient, or patient died AND has cancel date
      !!sym(att_status_o) %in% c("02", "03") & is.na(!!sym(cancellation_date_o)) ~ "missing",
      !!sym(att_status_o) %in% c("01", "05", "08", "09") & is.na(!!sym(cancellation_date_o)) ~ "missing but valid",
      (!!sym(att_status_o) == "99" | is.na(!!sym(att_status_o))) & !is.na(!!sym(cancellation_date_o)) ~ "invalid",
      TRUE ~ "missing but valid"
    ))
  
  return(df_cancellation_date)
  
}