############################################.
### Removes white space from data keys #####
############################################.

#Removes all whitespace from columns in data frame

# Author: Charlie Smith and Joana Bittencourt Silvestre
# Date: 16/11/23

# 1 Load libraries --------------------------------------------------------
# library(dplyr)
# library(stringr)

# 2 no_whitespace function -----------------------------------------------------

remove_spaces_data_keys <- function(df){
  df_without_spaces <- df %>% 
    mutate(across(all_of(c(!!ucpn_o, !!chi_o, !!upi_o)), 
                  ~ str_replace_all(.x, " ", "")))
  
  return(df_without_spaces)
  
}

