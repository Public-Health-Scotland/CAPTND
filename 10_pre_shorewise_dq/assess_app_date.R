
###############################.
### Assess Appointment Date ###
###############################.

# Author: Bex Madden
# Date: 2024-04-10

assess_app_date <- function(df){
  
  
  # assess values
  df_app_date <- df |> 
    mutate(check_app_date = case_when(
      is.na(!!sym(app_date_o)) ~ "missing",
      !!sym(app_date_o) >= "2015-01-01" & 
        !!sym(app_date_o) <= Sys.Date() ~ "valid",
      TRUE ~ "invalid"))
  
  return(df_app_date)
  
}