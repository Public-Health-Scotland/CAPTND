#####################################.
###  remove unusable records test ###
#####################################.

#Tests remove_unusable_records function


# 1 Load libraries, col names and functions ------------------------------------

source('setup/new_column_names_swift.R')
source('functions/not_tested/remove_unusable_records.R')

# 2 Load test dataset ----------------------------------------------------------

test_data=read_csv('../../../data/testDataset.csv') %>% 
  filter(issue %in% c('no_UCPN', 'no_CHI', 'ucpn_is_na', 'complete')) 

df_clean = remove_unusable_records(test_data)

#expected result: 3 removed records and 3 kept records
