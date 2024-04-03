
########################################################################################.
### Distinguish between preg_perinatal at referral and preg_perinatal at appointment ###
########################################################################################.

# preg_perinatal valid in referral and appointment records 

# Author: Charlie Smith
# Date: 2024-04-03

set_preg_perinatal_stage <- function(df){
  
  df_preg_peri <- df |> 
    mutate(
      # preg_peri referral
      !!preg_perinatal_ref_o := case_when(
      (!is.na(!!sym(ref_date_o)) | !is.na(!!sym(ref_rec_date_o))) & !is.na(!!sym(preg_perinatal_o)) ~ !!sym(preg_perinatal_o)),
      # preg_peri appointment
      !!preg_perinatal_app_o := case_when(
        !is.na(!!sym(app_date_o)) & !is.na(!!sym(preg_perinatal_o)) ~ !!sym(preg_perinatal_o))
      ) |> 
    select(-!!sym(preg_perinatal_o))
  
  return(df_preg_peri)
  
}