
################################.
### Assess lac status Edited ###
################################.

# Author: Charlie Smith
# Date: 2024-04-10

assess_lac_status_edited <- function(df){
  
  vec_protection <- read_xlsx(captnd_code_lookup, sheet = "Child_Protection") |>  # same codes as protection
    select(Codes) |> 
    mutate(Codes = str_pad(Codes, 2, pad = "0")) |>  
    filter(Codes != "99" ) |> 
    pull() |> as.numeric()
  
  df_lac <- df |> 
    mutate(check_looked_after_c_edited = case_when(
      is.na(!!sym(looked_after_c_edited_o)) ~ "missing",
      !!sym(looked_after_c_edited_o) %in% vec_protection ~ "valid",
      !!sym(looked_after_c_edited_o) == 99 ~ "not known",
      TRUE ~ "invalid"))
  
  return(df_lac)
  
}