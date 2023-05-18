####################################.
### Correct dob in test dataset  ###
####################################.

#Converts dob to character with format yyyy-mm-dd

# 1 Load packages ---------------------------------------------------------
library(dplyr)
library(lubridate)


# 2 Function --------------------------------------------------------------

dob_format <- function(df){
  
  df_dob_fix =df %>%
    mutate(dob = as.Date(dob, "%d/%m/%Y"))
  
  return(df_dob_fix)
}

