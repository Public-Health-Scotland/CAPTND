
#####################################.
### Assess referral received date ###
#####################################.

# Author: Charlie Smith
# Date: 2024-04-11


assess_ref_rec_date <- function(df){
  
  df_ref_rec_date <- df |> 
    mutate(check_ref_rec_date = case_when(
      is.na(!!sym(ref_rec_date_o)) ~ "missing",
      !!sym(ref_rec_date_o) >= "2015-01-01" & !!sym(ref_rec_date_o) <= Sys.Date() ~ "valid", 
      TRUE ~ "invalid"))
  
  return(df_ref_rec_date)
  
}