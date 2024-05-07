
#################################.
### Analyse Data Quality Data ###
#################################.

# Author: Charlie Smith
# Date: 2024-05-03


# ANALYSIS

# Phase 1

# load data
# filter and wrangle data (latest 15 months)
# get dq counts (and proportions?)
# save

source('./10_pre_shorewise_scripts/dq_reporting/load_dq_data.R')
source('./10_pre_shorewise_scripts/dq_reporting/filter_wrangle_data.R')
source('./10_pre_shorewise_scripts/dq_reporting/get_dq_counts.R')


df <- load_dq_data() |> 
  filter_wrangle_data() |> 
  get_dq_counts() |> 
  save_as_parquet(paste0(data_quality_report_dir, "/captnd_counts"))


# Phase 2
# complete missing values - maybe not needed?
# append submission status


