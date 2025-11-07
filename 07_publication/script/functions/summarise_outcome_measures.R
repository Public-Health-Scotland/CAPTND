#################################.
### For publication - CGI/PGI ###
#################################.

# Author: Luke Taylor
# Date: 2025-11-04

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

#### SETUP #####

# load data
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

# set constants
most_recent_month_in_data <- get_lastest_month_end(df) 

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# select vars
demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")







