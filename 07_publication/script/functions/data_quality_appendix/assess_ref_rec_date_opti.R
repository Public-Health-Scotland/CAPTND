
##########################################.
### Assess referral received date opti ###
##########################################.

# Author: Charlie Smith
# Date: 2024-12-03


assess_ref_rec_date_opti <- function(df){
  
  df_ref_rec_date_opti <- df |> 
    mutate(check_ref_rec_date_opti = case_when(
      is.na(!!sym(ref_rec_date_opti_o)) ~ "missing",
      !!sym(ref_rec_date_opti_o) >= "2015-01-01" & !!sym(ref_rec_date_opti_o) <= Sys.Date() ~ "valid", 
      TRUE ~ "invalid"))
  
  return(df_ref_rec_date_opti)
  
}