
#########################.
### Assess Protection ###
#########################. 

# Author: Charlie Smith
# Date: 2024-04-10

assess_protection <- function(df){
  
  vec_protection <- read_xlsx(captnd_code_lookup, sheet = "Child_Protection") |>  
    select(Codes) |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |>  
    filter(Codes != "99" ) |> 
    pull()
  
  df_protection <- df |> 
    mutate(check_protection = case_when(
             is.na(!!sym(protection_o)) ~ "Missing",
             !!sym(protection_o) %in% vec_protection ~ "Valid",
             !!sym(protection_o) == "99" ~ "Not known",
             TRUE ~ "Invalid"))
  
  return(df_protection)
  
}

