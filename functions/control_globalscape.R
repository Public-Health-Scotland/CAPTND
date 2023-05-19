
########################################################.
### Controller script for Consolidating Globalscape  ###
########################################################.

#This script makes connection to database, loads SWIFT data, merges CAMHS and PT,
#renames columns.

# 1 - Housekeeping --------------------------------------------------------
# 1.1 - Load packages -----------------------------------------------------

library(conflicted)
library(odbc)
library(rstudioapi)
library(plyr)
library(dbplyr)
library(dplyr)


# 1.2 Source column renamer function --------------------------------------
source('./functions/swift_column_renamer.R')
source('functions/globalscape_column_renamer.R')
source('functions/globalscape_data_loader.R')
source('functions/gather_globalscape_data.R')
source('functions/null_to_na.R')
source('functions/correct_HB_names.R')
source('functions/check_chi.R')
source('functions/remove_unusable_records.R')
source('functions/pad_chi.R')

# 1.3 - Deal with package conflicts ---------------------------------------
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)


# 2 - Gather globalscape --------------------------------------------------

df_glob_raw <- gather_globalscape()

# try functions against test data
# df_glob_raw <- read_csv("../../../data/testDataset_lowercase.csv") 

df_glob_cleaned <- list()
for(i in 1:length(df_glob_raw)){
  
  df_glob_cleaned_partial <- df_glob_raw[[i]] %>% 
    null_to_na() %>% 
    correct_hb_names() %>% 
    pad_chi()
    #check_chi_captnd() %>% 
    #remove_unusable_records() 
  
  df_glob_cleaned[[i]] <- df_glob_cleaned_partial
  
}

df_glob_merged <- df_glob_cleaned %>% 
  reduce(full_join, by = c('ucpn', 
                           'chi', 
                           'hb_name', 
                           'dataset_type'))



check <- anti_join(df_glob_raw, df_glob_cleaned)


glob_df <- captnd_all %>% 
  reduce(full_join, by = c('ucpn', 
                           'chi', 
                           'hb_name', 
                           'dataset_type')) %>% 
  arrange(ucpn)













