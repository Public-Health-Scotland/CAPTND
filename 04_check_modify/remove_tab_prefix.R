#############################.
###   Remove Tab _prefix  ###
#############################.

#Removes "\t" prefix on Ayrshire and Arran referral records
#This issue was noticed 06/10/2025

#Author: Luke Taylor

# 1 Load packages ---------------------------------------------------------
# library(dplyr)


# 2 Function --------------------------------------------------------------

remove_tab_prefix <- function(df){
  
  remove_tab_prefix <- df %>%
    mutate(ucpn = str_replace_all(ucpn, "\t", ""))
  
  return(remove_tab_prefix)
  
}
