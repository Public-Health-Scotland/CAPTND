
#############################################.
### Label retired variable proportions NA ###
#############################################.

# Author: Charlie Smith
# Date: 2024-05-08

label_retired_variables_na <- function(df){
  
  df_retired_na <- df |> 
    ungroup() |> 
    mutate(proportion = case_when(
      variable %in% c("outcome_1", "outcome_2", "outcome_3") & 
        header_date_month >= "2023-10-01" ~ NA_real_,
      TRUE ~ proportion)) 
  
  return(df_retired_na)
  
}
