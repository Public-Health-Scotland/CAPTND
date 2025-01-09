

###########################.
### Assess DOB Verified ###
###########################.

# Author: Charlie Smith
# Date: 2024-12-03


# DOB - shouldn't be in the future or over 110 years ago

assess_dob_verified <- function(df){
  
  df_dob <- df |> 
    mutate(check_dob_verified = case_when(
      !!sym(dob_verified_o) >= Sys.Date()-years(110) & !!sym(dob_verified_o) <= Sys.Date() ~ "valid", # 110 years ago to present is allowed
      is.na(!!sym(dob_verified_o)) ~ "missing",
      TRUE ~ "invalid"))
  
  return(df_dob)
  
}

