
########################.
### Set column order ###
########################.

# Author: Charlie Smith
# Date: 2024-10-03

set_column_order <- function(df){
  
  df_ordered <- df |> 
    select(any_of(c(
      header_date_o,
      data_keys, 
      vec_demographic_cols,
      vec_referral_cols,
      vec_app_cols,
      vec_diag_cols,
      vec_treat_cols,
      vec_outcome_cols,
      vec_case_closed_cols)), 
      everything())
  
  return(df_ordered)
  
}

# test <- set_column_order(df)



