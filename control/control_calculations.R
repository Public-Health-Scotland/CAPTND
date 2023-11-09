
###########################################.
### Controller script for calculations  ###
###########################################.

#This takes the last rtt evaluated file and calculates different measure from it


# 1.1 - Load packages -----------------------------------------------------

library(dplyr)
library(arrow)
library(purrr)
library(stringr)
library(lubridate)
library(plyr)
library(conflicted)
conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')

# 1.2 - Source functions -------------------------------------------------
source('config/new_colnames.R')
source('setup/open_last_parquet_with_rrt_eval.R')
source('calculations/calculate_open_cases.R')
source('calculations/calculate_patients_waiting.R')
source('calculations/calculate_patients_seen.R')
source('calculations/calculate_referrals.R')


# 2 - open most recent RTT eval file--------------------------------------

df <- open_last_parquet_with_rrt_eval()['df'][[1]]

last_date_on_file <-open_last_parquet_with_rrt_eval()['date'][[1]]

# 2.1 calculate variables -------------------------------------------------

calculate_open_cases(df, last_date_on_file)
calculate_patients_waiting(df, last_date_on_file) 
calculate_patients_seen(df, last_date_on_file)
calculate_referrals(df, last_date_on_file)


