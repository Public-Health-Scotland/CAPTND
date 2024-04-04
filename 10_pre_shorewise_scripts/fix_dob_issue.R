
##########################.
### Fix issue with DOB ###
##########################.

# Author: Charlie Smith
# Date: 2024-04-03

# Purpose: DOB is allowed in referral and discharge records, however this is only needed for NHS 24,
# Therefore DOB for discharge can be removed unless the HB is NHS 24

fix_dob_issue <- function(df){
  
  df_dob <- df |> 
    mutate(!!sym(dob_o) := case_when(
      !!sym(hb_name_o) == "NHS 24" ~ !!sym(dob_o),
      !!sym(hb_name_o) != "NHS 24" & !is.na(!!sym(case_closed_date_o)) & !is.na(!!sym(dob_o)) ~ NA_Date_,
      TRUE ~ !!sym(dob_o)
    ))
  
  return(df_dob)
  
}
