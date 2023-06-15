
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
library(dbplyr)
library(purrr)
library(stringr)

# 1.2 Source functions --------------------------------------
source('./functions/swift_column_renamer.R')
source('functions/globalscape_column_renamer.R')
source('functions/globalscape_data_loader.R')
source('functions/save_df_as_parquet.R')
source('functions/save_globalscape_parquet.R')
source('functions/null_to_na.R')
source('functions/correct_HB_names.R')
source('functions/check_chi.R')
source('functions/remove_unusable_records.R')
source('functions/pad_chi.R')
source('functions/access_glob_parquet_files.R')
source('functions/set_col_data_types.R')
source('functions/complete_sex_from_chi.R')
source('functions/not_tested/load_test_data.R')
source('functions/not_tested/append_postcode_lookup.R')
library(plyr)
library(dplyr)


# 1.3 - Deal with package conflicts ---------------------------------------
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)


# 2 - Gather globalscape --------------------------------------------------

#commented out because the files have already been saved to parquet
#df_glob_raw <- save_globalscape_parquet()

#load saved parquet files
df_glob_raw <- load_glob_parquet_dfs()

# try functions against test data
 # df_glob_raw <- list(read_csv("../../../data/testDataset_lowercase.csv"))
 # names(df_glob_raw)=c('test')

 cleaning_fun <- list(null_to_na, correct_hb_names, pad_chi)

 df_glob_clean <- df_glob_raw %>% 
   map(cleaning_fun) %>%
   map2(.,names(.), check_chi_captnd) %>%
   map2(., names(.), remove_unusable_records) %>%
   map(~select(.x, -!!sym(upi_o))) %>%
   map(~ .x %>% mutate(across(where(is.character), trimws))) # is this okay here or on line 77?
   
x <- df_glob_clean[[1]] 
y <- df_glob_raw[[1]]
z <- anti_join(x, y) %>% 
  select(where(is.character))


df_glob_merged <- df_glob_clean %>% 
  reduce(full_join, by = c(ucpn_o, 
                           chi_o, 
                           hb_name_o, 
                           dataset_type_o,
                           'sub_source')) %>% # turn sub_source into object
  set_col_data_types() %>%  # problem here - looking for swift column names that are not in globalscape
  append_postcode_lookup()
  
rm(cleaning_fun, df_glob_clean, df_glob_raw)  




