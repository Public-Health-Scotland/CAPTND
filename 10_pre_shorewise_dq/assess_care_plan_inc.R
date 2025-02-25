
##################################.
### Assess Care Plan Inclusion ###
##################################.

# Author: Charlie Smith
# Date: 2025-02-19

assess_care_plan_inc <- function(df){
  
  df_care_plan_inc <- df |> 
    mutate(check_care_plan_inc = case_when(
      !!sym(care_plan_inc_o) %in% c("01", "02") ~ "valid",
      !!sym(care_plan_inc_o) == "99" ~ "not known",
      is.na(!!sym(care_plan_inc_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_care_plan_inc)
  
  
}


