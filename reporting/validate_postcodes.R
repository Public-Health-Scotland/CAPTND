
##########################.
### Validate postcodes ###
##########################.

# Purpose: check validity of captnd postcodes based on postcode index file (list of current and retired postcodes)
# Author: Charlie Smith
# Date: 2023-06-20


# 1.1 - Load packages -------------------------------------------------------
# library(tidyr)
# library(magrittr)
# library(rio)
# library(dplyr)


# 1.2 - Load functions ----------------------------------------------------

source('check_modify/clean_postcode.R')


# 2 - Write function ------------------------------------------------------

validate_postcode <- function(data){
  
  # use SIMD lookup as basic valid postcode list 
  postcode_lookup <- import("../../../data/postcode_simd_lookup.csv") %>% 
    select(1) %>% 
    mutate(postcode_status = "valid") # label all as valid
  
  # tidy postcodes in df, left_join index, summarise validity counts and proportion
  x <- data %>% 
    filter(!!sym(record_type_o) == "referral") %>%  # postcode only in referral records currently
    select(sym(dataset_type_o), sym(hb_name_o), sym(postcode_o)) %>% 
    mutate(!!sym(postcode_o) := str_replace_all(!!sym(postcode_o), " ", ""), # remove spaces from postcodes
           !!sym(postcode_o) := toupper(!!sym(postcode_o))) %>% 
    clean_postcode() %>% 
    left_join(., postcode_lookup, by = postcode_o, multiple = "all") %>% 
    mutate(postcode_status = case_when(
       !is.na(!!sym(postcode_o)) & is.na(postcode_status)  ~ "invalid",
       is.na(!!sym(postcode_o)) & is.na(postcode_status) ~ "missing",
       TRUE ~ postcode_status)) %>% 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), postcode_status) %>%
    dplyr::summarise(count = n()) %>% 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) %>% 
    mutate(
      total = sum(count),
      prop = round(count/total*100, 1)) #%>% 
    #filter(postcode_status == "invalid") # look for high invalid prop
  
  return(x)
  
}

df_test <- validate_postcode(df_glob_merged)

# Note:
# currently only useful for referral records as only these contain postcodes
# was globalscape consolidation method supposed to ensure full patient-level distribution of referral info?




