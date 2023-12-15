
########################################.
### Append postcode-simd lookup file ###
########################################.

# Purpose: Add SIMD info to captnd based on postcode
# Author: Charlie Smith
# Date: 2023-06-20


# 1 - Load packages -------------------------------------------------------
# library(tidyr)
# library(dplyr)
# library(magrittr)
# library(rio)
# conflicts_prefer(dplyr::rename)



# 2 - Write function ------------------------------------------------------

append_postcode_lookup <- function(data){
  
  postcode_lookup <- import("../../../data/postcode_simd_lookup.csv") %>% 
    rename(!!postcode_last_reported_o := !!postcode_o)
  
  x <- data %>% 
    mutate(!!sym(postcode_last_reported_o) := str_replace_all(!!sym(postcode_last_reported_o), " ", ""), # remove spaces from postcodes
           !!sym(postcode_last_reported_o) := toupper(!!sym(postcode_last_reported_o))) %>% # make postcodes all caps
    left_join(., postcode_lookup, by = postcode_last_reported_o) 
  
  return(x)
  
}
