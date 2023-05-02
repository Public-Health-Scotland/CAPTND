######################.
### check chi test ###
######################.

#Tests check chi function


# 1 Load libraries, col names and functions ------------------------------------

source('setup/new_column_names_swift.R')
source('testing/check_chi.R')

# 2 Load test dataset ----------------------------------------------------------

df_test <-  read_csv('../../../data/testDataset.csv') %>% 
  mutate(chi = as.character(chi)) %>% 
  mutate(chi := chi_pad(chi))

df_clean <- check_chi_captnd(df_test)

#expected result: 4 removed records and 74 kept records
