###############################.
### Assess Case Closed Date ###
###############################.

# Author: Bex Madden
# Date: 2024-04-10

assess_case_closed_date <- function(df){
  
  # assess values
  df_case_closed <- df |> 
    mutate(check_case_closed_date = case_when(
      is.na(!!sym(case_closed_date_o)) ~ "missing",
      !!sym(case_closed_date_o) >= "2015-01-01" & 
        !!sym(case_closed_date_o) <= Sys.Date() ~ "valid",
      TRUE ~ "invalid"))
  
  return(df_case_closed)
  
}