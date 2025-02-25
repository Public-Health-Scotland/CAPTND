
###################################.
### Assess reason for treatment ###
###################################.

# Author: Charlie Smith
# Date: 2025-02-20


assess_treat_reason <- function(df){
  
  df_treat_reason <- df |> 
    mutate(check_treat_reason_1 = case_when(
      
      is.na(!!sym(treat_reason_1_o)) ~ "missing",
      TRUE ~ "valid"),
      
      check_treat_reason_2 = case_when(
        
        is.na(!!sym(treat_reason_2_o)) ~ "missing",
        TRUE ~ "valid"),
      
      check_treat_reason_3 = case_when(
        
        is.na(!!sym(treat_reason_3_o)) ~ "missing",
        TRUE ~ "valid")
      
      )
  
  return(df_treat_reason)
  
}