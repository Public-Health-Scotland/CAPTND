#####################.
###   NULL to NA  ###
#####################.

#Converts NULL strings into NAs

# 1 Load packages ---------------------------------------------------------
# library(dplyr)


# 2 Function --------------------------------------------------------------

null_to_na <- function(df){
  
  df_no_ull <- df %>%
    mutate(across(where(is.character), \(x) na_if(x,"NULL")))
  
  return(df_no_null)
  
}

