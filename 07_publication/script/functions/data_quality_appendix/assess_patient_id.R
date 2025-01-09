
#########################.
### Assess Patient ID ###
#########################.

# Author: CJS 
# Date: 2024-12-03

assess_patient_id <- function(df){
  
  df_pat_id <- df |> 
    mutate(check_patient_id = case_when(
             is.na(!!sym(patient_id_o)) ~ "missing",
             TRUE ~ "valid"))
  
  return(df_pat_id)
  
}

