
#################################################################.
### Get all Globalscape stages and save them as parquet files ###
#################################################################.

#This script makes connection to database, loads globalscape data, merges CAMHS and PT,
#renames columns, saves each stage as parquet to be used in control globalscape script.

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
#source('functions/not_tested/load_test_data.R')
source('functions/append_postcode_lookup.R')
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


