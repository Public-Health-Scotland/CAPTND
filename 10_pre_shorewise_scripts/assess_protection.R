
################################.
### Assess Protection Status ###
################################.

# Author: Charlie Smith
# Date: 2024-09-11


assess_protection <- function(df){
  
  vec_pro <- read_xlsx(captnd_code_lookup, sheet = "Child_Protection") |> 
    select(Codes) |> 
    filter(Codes != "99") |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |> 
    pull()
  
  df_pro <- df |> 
    mutate(check_protection = case_when(
             is.na(!!sym(protection_o)) ~ "missing",
             !!sym(protection_o) %in% vec_pro ~ "valid",
             !!sym(protection_o) == "99" ~ "not known",
             TRUE ~ "invalid"))
  
  return(df_pro)
  
}
