
##########################.
### Validate postcodes ###
##########################.



# 1 - Load packages -------------------------------------------------------
library(tidyr)
library(magrittr)
library(rio)
library(dplyr)

# 2 - Write function ------------------------------------------------------

validate_postcode <- function(data){
  
  # load small area postcode file (contain existing and deleted postcodes)
  path <- "../../../data/small_area_postcode_index_23_1.csv" 
  
  postcode_index <- import(path, format = "csv") %>% 
    select(1) %>% 
    rename_with(tolower) %>% 
    mutate(postcode = str_replace(postcode, " ", ""),
           status = "valid") %>% 
    unique()

  x <- data %>% 
    select(sym(dataset_type_o), sym(hb_name_o), sym(postcode_o)) %>% 
    mutate(!!sym(postcode_o) := str_replace(!!sym(postcode_o), " ", "")) %>%  # remove spaces from postcodes
    left_join(., postcode_index, by = postcode_o) %>% 
    mutate(status = case_when(
       !is.na(!!sym(postcode_o)) & is.na(status)  ~ "invalid postcode",
       is.na(!!sym(postcode_o)) & is.na(status) ~ "no postcode",
       TRUE ~ status)) %>% 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), status) %>%
    dplyr::summarise(count = n()) %>% 
    ungroup() %>%
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) %>% 
    mutate(
      total = sum(count),
      prop = round(count/total*100, 1))
  
  return(x)
  
}

df_test <- validate_postcode(data = df_glob_merged %>% filter(record_type == "referral")) 

# Note:
# currently only useful for referral records as only these contain postcodes
# was join method supposed to ensure full patient-level distribution of referral info?




