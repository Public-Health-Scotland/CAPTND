###################################.
###   Compare DOB and CHI test  ###
###################################.


# 1 Load function ----------------------------------------------------------

source('functions/testing/check_dob_from_chi.R')
library(readr)
library(dplyr)


# 2 Test --------------------------------------------------------------

df=read_csv('../../../data/testDataset.csv') %>% 
  filter(issue %in% c('invalid_chi','silly_dob','no_CHI', 'postcode_incomplete', 'no_demographic_info_for_appointments')) %>% 
  mutate(dob = as.Date(dob,
                       format = "%d/%m/%Y"))

df_test=check_dob_from_chi(df)

#expected: 2 NAs, 5 TRUE, 1 FALSE


