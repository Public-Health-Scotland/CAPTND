
###################################.
### Assess Treatment Start Date ###
###################################.

# Author: Bex Madden
# Date: 2024-04-10

assess_treat_start_date <- function(df){

  # assess values
  df_treat_start <- df |> 
    mutate(check_treat_start_date = case_when(
      is.na(!!sym(treat_start_date_o)) ~ "missing",
      !!sym(treat_start_date_o) >= "2015-01-01" & 
        !!sym(treat_start_date_o) <= Sys.Date() ~ "valid",
      TRUE ~ "invalid"))
  
  return(df_treat_start)
  
}

