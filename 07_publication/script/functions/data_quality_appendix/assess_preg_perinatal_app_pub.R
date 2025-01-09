
#########################################.
### Assess preg_perinatal app for pub ###
#########################################. 

# Author: Charlie Smith
# Date: 2024-12-04

assess_preg_perinatal_app_pub <- function(df){
  
  vec_preg_perinatal <- read_xlsx(captnd_code_lookup, sheet = "Pregnancy") |> 
    select(Code) |> 
    filter(Code != "99") |> 
    mutate(Code = str_pad(Code, 2, pad = "0")) |> 
    pull() |> as.numeric()
  
  df_preg_perinatal <- df |> 
    mutate(check_preg_perinatal_app = if_else(!!sym(dataset_type_o) == "PT", case_when(
      is.na(!!sym(preg_perinatal_app_o)) ~ "missing",
      !!sym(preg_perinatal_app_o) %in% vec_preg_perinatal ~ "valid",
      !!sym(preg_perinatal_app_o) == 99 ~ "not known",
      TRUE ~ "invalid"), NA_character_))
  
  return(df_preg_perinatal)
  
}
