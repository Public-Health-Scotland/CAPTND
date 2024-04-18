
#############################.
### Assess Rejection Date ###
#############################.

# Author: Charlie Smith
# Date: 2024-04-11

assess_rej_date <- function(df){
  
  df_rej_date <- df |> 
    mutate(check_ref_rej_date = case_when(
      !!sym(ref_acc_o) == '01' & is.na(!!sym(ref_rej_date_o)) ~ "missing but valid", # accepted & no rej date = rej date is valid
      !!sym(ref_acc_o) == '02' & !is.na(!!sym(ref_rej_date_o)) ~ "valid", # rejected & rej date = rej date is valid
      !is.na(!!sym(ref_rej_date_o)) & !!sym(ref_rej_date_o) >= !!sym(ref_rec_date_o) ~ "valid", # has a rej date that is after referral date is valid
      !!sym(ref_acc_o) == '02' & is.na(!!sym(ref_rej_date_o)) ~ "missing", # rejected no rej date is blank = missing rej_date
      !!sym(ref_acc_o) == '03' & is.na(!!sym(ref_rej_date_o)) ~ "missing but valid", # pending and no rejection date = rej_date is valid
      is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_rej_date_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_rej_date)
  
}
