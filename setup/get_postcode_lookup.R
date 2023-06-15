
#######################################.
### Get latest postcode lookup file ###
#######################################.


# Purpose: Access Scottish Index of Multiple Deprivation 2020v2 postcode lookup file
# Author: Charlie Smith
# Date: 2023-06-13


# 1 - Loadpackages --------------------------------------------------------
library(rio)
library(dplyr)
library(magrittr)
library(stringr)


# 2 - Get data ------------------------------------------------------------
url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-postcode-look-up-file/documents/simd-2020-postcode-lookup-v5/simd-2020-postcode-lookup-v5/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bpostcode%2Blookup%25232.xlsx"

postcode_lookup <- import(url, format = "xlsx", which = "All postcodes") %>% 
  select(-c(2,3)) %>% 
  rename_with(tolower) %>% 
  mutate(postcode = str_replace(postcode, " ", "")) # remove whitespace from postcode values


# 3 - Save lookup file ----------------------------------------------------
export(postcode_lookup, "../../../data/postcode_simd_lookup.csv")


# 4 - Clear GE ------------------------------------------------------------
#rm(list = ls())
