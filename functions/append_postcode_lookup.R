
########################################.
### Append postcode-simd lookup file ###
########################################.



# 1 - Load packages -------------------------------------------------------
library(tidyr)
library(dplyr)
library(magrittr)
library(rio)


# 2 - Write function ------------------------------------------------------

append_postcode_lookup <- function(data){
  
  postcode_lookup <- import("../../../data/postcode_simd_lookup.csv")
  
  x <- data %>% 
    mutate(!!sym(postcode_o) := str_replace_all(!!sym(postcode_o), " ", ""), # remove spaces from postcodes
           !!sym(postcode_o) := toupper(!!sym(postcode_o))) %>% # make postcodes all caps
    left_join(., postcode_lookup, by = postcode_o)
  
  return(x)
  
}
