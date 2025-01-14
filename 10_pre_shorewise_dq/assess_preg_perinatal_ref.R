
#################################.
### Assess preg_perinatal_ref ###
#################################. 

# Author: Charlie Smith
# Date: 2024-04-10

assess_preg_perinatal_ref <- function(df){
  
  vec_preg_perinatal_ref <- read_xlsx(captnd_code_lookup, sheet = "Pregnancy") |> 
    select(Code) |> 
    filter(Code != "99") |> 
    mutate(Code = str_pad(Code, 2, pad = "0")) |> 
    pull()
  
  df_preg_perinatal_ref <- df |> 
    mutate(check_preg_perinatal_ref = if_else(!!sym(dataset_type_o) == "PT", case_when(
      is.na(!!sym(preg_perinatal_ref_o)) ~ "missing",
      !!sym(preg_perinatal_ref_o) %in% vec_preg_perinatal_ref ~ "valid",
      !!sym(preg_perinatal_ref_o) == "99" ~ "not known",
      TRUE ~ "invalid"), NA_character_))
  
  return(df_preg_perinatal_ref)
  
}
