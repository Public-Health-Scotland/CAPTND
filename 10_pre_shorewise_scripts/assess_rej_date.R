
#############################.
### Assess Rejection Date ###
#############################.

# Author: Charlie Smith
# Date: 2024-04-11

assess_rej_date <- function(df){
  
  df_rej_date <- df
    mutate(check_rej_date = case_when(
      # !!sym(ref_acc_o) %in% c('01', '02') & is.na(!!sym(ref_rej_date_o)) |
      #   is.na(!!sym(ref_acc_o)) & is.na(!!sym(ref_rej_date_o)) ~ "missing but valid",
      # !!sym(ref_acc_o) == '02' &
      #   !is.na(!!sym(ref_rej_date_o)) &
      #   !!sym(ref_rej_date_o) >= !!sym(ref_rec_date_o) ~ "valid",
      # !!sym(ref_acc_o) == '02' & is.na(!!sym(ref_rej_date_o)) ~ "missing", 
      # TRUE ~ "invalid")
      )
  
  return(df_rej_date)
  
}


