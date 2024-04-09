
##################.
### Assess DOB ###
##################.

# Author: Charlie Smith
# Date: 2024-04-05


# DOB - shouldn't be in the future or over 110 years ago

assess_dob <- function(df){
  
  df_dob <- df |> 
    mutate(check_dob = case_when(
      !!sym(dob_o) >= Sys.Date()-years(110) & !!sym(dob_o) <= Sys.Date() ~ "valid", # 110 years ago to present is allowed
      is.na(DOB) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_dob)
    
}



