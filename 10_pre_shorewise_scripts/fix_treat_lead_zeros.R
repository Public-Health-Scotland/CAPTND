
################################################.
### Remove leading zeros from treatment info ###
################################################.

# Author: Charlie Smith
# Date: 2024-07-01


fix_treatment_leadings_zeros <- function(df){
  
  df_test <- df |> 
    mutate(
      !!sym(treat_1_o) := case_when(
      !!sym(treat_1_o) == "096" ~ "96",
      !!sym(treat_1_o) == "098" ~ "98",
      !!sym(treat_1_o) == "099" ~ "99",
      TRUE ~ !!sym(treat_1_o)),
      
      !!sym(treat_2_o) := case_when(
        !!sym(treat_2_o) == "096" ~ "96",
        !!sym(treat_2_o) == "098" ~ "98",
        !!sym(treat_2_o) == "099" ~ "99",
        TRUE ~ !!sym(treat_2_o)),
        
        !!sym(treat_3_o) := case_when(
          !!sym(treat_3_o) == "096" ~ "96",
          !!sym(treat_3_o) == "098" ~ "98",
          !!sym(treat_3_o) == "099" ~ "99",
          TRUE ~ !!sym(treat_3_o)))
  
  return(df_test)
  
}
