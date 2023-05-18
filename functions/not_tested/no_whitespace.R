#############################.
### Removes whitespace  #####
#############################.

#Removes all whitespace from columns in data frame

# Author: Maria Gannon
# Date: 17/04/2022

# 1 Load libraries --------------------------------------------------------
library(dplyr)
library(stringr)

# 2 no_whitespace function -----------------------------------------------------

no_whitespace <- function(df){
                     
                     df_no_whitespace <-
                       as.data.frame(apply(df, 2, str_remove_all, " "))
                    
                     return(df_no_whitespace)
}


