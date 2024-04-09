
##################.
### Assess UPI ###
##################.

# Author: Charlie Smith
# Date: 2024-04-05

assess_upi <- function(df){
  
  df_upi <- df |> 
    mutate(check_upi = case_when(
      is.na(!!sym(upi_o)) ~ "missing",
      TRUE ~ "valid"))
  
  return(df_upi)
  
}