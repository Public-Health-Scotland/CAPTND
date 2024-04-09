
###################.
### Assess UCPN ###
###################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_ucpn <- function(df){
  
  df_ucpn <- df |> 
    mutate(check_ucpn = case_when(
      is.na(!!sym(ucpn_o)) ~ "missing",
      !!sym(ucpn_o) == "NULL" ~ "missing",
      !!sym(ucpn_o) == "NA" ~ "missing",
      nchar(!!sym(ucpn_o)) <= 20 ~ "valid", # problem for using UPI as UCPN - impute this later in code!!!
      TRUE ~ "invalid"))
  
  return(df_ucpn)
  
}