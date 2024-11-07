
############################################.
### Assess Unavailability Dates and Days ###
############################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_unav_dates <- function(df){
  

  # assess values
  df_unav_dates <- df |> 
    mutate(check_unav_date_start = case_when(
             is.na(!!sym(unav_date_start_o)) ~ "missing",
             !!sym(unav_date_start_o) >= "2015-01-01" & 
               !!sym(unav_date_start_o) <= Sys.Date() ~ "valid",
             TRUE ~ "invalid"),
           
           check_unav_date_end = case_when(
             is.na(!!sym(unav_date_end_o)) ~ "missing",
             !!sym(unav_date_end_o) >= "2015-01-01" & 
               !!sym(unav_date_end_o) >= !!sym(unav_date_start_o) ~ "valid",
             TRUE ~ "invalid"),
           
           check_unav_days_no = case_when(
             is.na(!!sym(unav_days_no_o)) ~ "missing",
             !!sym(unav_days_no_o) >= 0 ~ "valid",
             TRUE ~ "invalid"))
  
  return(df_unav_dates)
  
}