
#############################.
### Assess Age at Ref Rec ###
#############################.

# Author: CJS
# Date: 2024-12-03


assess_age_at_ref_rec <- function(df){
  
  df_age <- df |> 
    mutate(check_age_at_ref_rec = case_when(
      is.na(!!sym(age_at_ref_rec_o)) ~ "missing",
      !!sym(age_at_ref_rec_o) >= 0 & !!sym(age_at_ref_rec_o) <= 110 ~ "valid",
      TRUE ~ "invalid"))
  
  return(df_age)
  
}



