
#################################################################.
### Get all Globalscape stages and save them as parquet files ###
#################################################################.

#This script makes connection to database, loads globalscape data, merges CAMHS and PT,
#renames columns, saves each stage as parquet to be used in control globalscape script.

# 1 - Housekeeping --------------------------------------------------------
# 1.1 - Load packages -----------------------------------------------------

# library(conflicted)
# library(odbc)
# library(rstudioapi)
# library(dbplyr)
# library(purrr)
# library(stringr)

# 1.2 Source functions --------------------------------------
source('./setup/swift_column_renamer.R')
source('globalscape_prep/globalscape_column_renamer.R')
source('globalscape_prep/globalscape_data_loader.R')
source('setup/save_df_as_parquet.R')
source('globalscape_prep/save_globalscape_parquet.R')
source('setup/null_to_na.R')
source('check_modify/correct_HB_names.R')
source('check_modify/check_chi.R')
source('check_modify/remove_unusable_records.R')
source('check_modify/pad_chi.R')
source('globalscape_prep/access_glob_parquet_files.R')
source('setup/set_col_data_types.R')
source('check_modify/check_sex_from_chi.R')
#source('functions/not_tested/load_test_data.R')
source('config/get_postcode_lookup.R')
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
df_glob_raw <- save_globalscape_parquet()


