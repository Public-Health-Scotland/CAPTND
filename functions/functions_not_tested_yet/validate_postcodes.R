
##########################.
### Validate postcodes ###
##########################.

# Purpose: check validity of captnd postcodes based on postcode index file (list of current and retired postcodes)
# Author: Charlie Smith
# Date: 2023-06-20


# TIDY THIS UP...

# 1.1 - Load packages -------------------------------------------------------
library(tidyr)
library(magrittr)
library(rio)
library(dplyr)


# 1.2 - Load functions ----------------------------------------------------

source("./functions/functions_not_tested_yet/clean_postcode.R")



# 2 - Write function ------------------------------------------------------

validate_postcode <- function(data){
  
  # # load small area postcode file (contains existing and deleted postcodes)
  # path <- "../../../data/small_area_postcode_index_23_1.csv" 
  # 
  # postcode_index <- import(path, format = "csv") %>% 
  #   select(1) %>% 
  #   rename_with(tolower) %>% 
  #   mutate(postcode = str_replace_all(postcode, " ", ""), # remove all spaces in postcode
  #          postcode2 = if_else(
  #            nchar(postcode) == 8 & str_sub(postcode, -1) %in% c("A", "B", "C", "D"), # if postcode has 8 characters & ends with A, B, C, D
  #            str_sub(postcode, end = -2), # remove last character
  #            postcode), # otherwise use original postcode
  #          chars = nchar(postcode)) %>% 
  #   group_by(postcode2) %>% 
  #   mutate(multi = if_else(sum(row_number()) == 1, 0, 1)) %>% # flag multiples of postcode2 
  #   ungroup()
  #   
  # postcode_multies <- postcode_index %>% # get multi-postcodes to index to account for 8 characetr postcodes ending in A, B, C, etc. 
  #   filter(multi == 1) %>% select(postcode2) %>% 
  #   rename(postcode = postcode2) %>% 
  #   unique() 
  # 
  # postcode_index <- postcode_index %>% 
  #   select(postcode) %>% 
  #   rbind(., postcode_multies) %>%  # append multi-postcodes to index
  #   mutate(postcode_status = "valid")
  
  # use SIMD lookup as basic valid postcode list 
  postcode_lookup <- import("../../../data/postcode_simd_lookup.csv") %>% 
    select(1) %>% 
    mutate(postcode_status = "valid")
  
  # tidy postcodes in df, left_join index, summarise validity counts and proportion
  x <- data %>% 
    filter(!!sym(record_type_o) == "referral") %>%  # postcode only in referral records currently
    select(sym(dataset_type_o), sym(hb_name_o), sym(postcode_o)) %>% 
    mutate(!!sym(postcode_o) := str_replace_all(!!sym(postcode_o), " ", ""), # remove spaces from postcodes
           !!sym(postcode_o) := toupper(!!sym(postcode_o)), # make postcodes all caps
           #!!sym(postcode_o) := iconv(!!sym(postcode_o), from = 'UTF-8', to = 'ASCII//TRANSLIT'), # convert to ASCII to remove accents
           #!!sym(postcode_o) := str_replace_all(!!sym(postcode_o), "[^[:alnum:]]", "")# remove all non-alphanumeric characters
           ) %>% 
    clean_postcode() %>% 
    #left_join(., postcode_index, by = postcode_o, multiple = "all") %>% 
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




