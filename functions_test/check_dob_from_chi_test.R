###################################.
###   Compare DOB and CHI test  ###
###################################.


# 1 Load function ----------------------------------------------------------

source('testing/check_dob_from_chi.R')
library(readr)
library(dplyr)


# 2 Test --------------------------------------------------------------

df=read_csv('../../../data/testDataset.csv') %>% 
  filter(issue %in% c('invalid_chi','silly_dob','no_CHI', 'postcode_incomplete'))

df_test=check_dob_from_chi(df)

#expected: 2 NAs, 1 TRUE, 1 FALSE
