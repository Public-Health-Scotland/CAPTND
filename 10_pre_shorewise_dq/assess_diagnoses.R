############################.
### Assess Diagnosis 1-3 ###
############################.

# Author: Bex Madden
# Date: 2024-04-10

assess_diagnoses <- function(df){
  
  # assess values
  df_diagnosis <- df |> 
    mutate(check_diag_1 = case_when(
      is.na(!!sym(diag_1_o)) ~ "missing",
      TRUE ~ "valid"),
      check_diag_2 = case_when(
        is.na(!!sym(diag_2_o)) ~ "missing",
        TRUE ~ "valid"),
      check_diag_3 = case_when(
        is.na(!!sym(diag_3_o)) ~ "missing",
        TRUE ~ "valid"))
  
  return(df_diagnosis)
  
}
