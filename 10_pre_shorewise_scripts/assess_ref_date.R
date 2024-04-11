
############################.
### Assess referral date ###
############################.

# Author: Charlie Smith
# Date: 2024-04-11


assess_ref_date <- function(df){
  
  df_ref_date <- df |> 
    mutate(check_ref_date = case_when(
      is.na(!!sym(ref_date_o)) ~ "missing",
      !!sym(ref_date_o) >= "2015-01-01" & !!sym(ref_date_o) <= Sys.Date() ~ "valid", 
      TRUE ~ "invalid"))
  
  return(df_ref_date)
  
}