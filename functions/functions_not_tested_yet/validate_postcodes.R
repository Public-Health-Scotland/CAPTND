
##########################.
### Validate postcodes ###
##########################.



# 1 - Load packages -------------------------------------------------------
library(tidyr)
library(magrittr)
library(rio)
library(dplyr)

# 2 - Write function ------------------------------------------------------

# this is not working well :(
# Postcodes flagged as invalid still valid in royal mail postcode checker... 

validate_postcode <- function(data){
  
  path <- "../../../data/large_postcode_index_23_1.csv"
  
  postcode_index <- import(path, format = "csv") %>% 
    select(1) %>% 
    rename_with(tolower) %>% 
    mutate(postcode = str_replace(postcode, " ", ""),
           status = "valid")  

  x <- data %>% 
    select(sym(hb_name_o), sym(postcode_o)) %>% 
    mutate(!!sym(postcode_o) := str_replace(!!sym(postcode_o), " ", "")) %>%  # remove spaces from postcodes
    left_join(., postcode_index, by = postcode_o) %>% 
    mutate(status = case_when(
       !is.na(!!sym(postcode_o)) & is.na(status)  ~ "invalid postcode",
       is.na(!!sym(postcode_o)) & is.na(status) ~ "no postcode",
       TRUE ~ status)) %>% 
    ungroup() %>% 
    group_by(!!sym(hb_name_o), status) %>% 
    dplyr::summarise(count = n())
    
  return(x)
  
}
