
#########################################.
### Create CAPTND Data Quality Report ###
#########################################.

# Author: Charlie Smith
# Date: 2024-05-03

# Aim to recreate pre-shorewise DQ reporting using Shorewise data

### Structure

# load packages

# load functions
source('./10_pre_shorewise_scripts/dq_reporting/load_functions.R')

# set constants
source('./10_pre_shorewise_scripts/dq_reporting/set_constants.R')

# data prep
source('./10_pre_shorewise_scripts/dq_reporting/data_prep.R')

# create dq charts
source('./10_pre_shorewise_scripts/dq_reporting/create_dq_charts.R')

# summarise submissions
source('./10_pre_shorewise_scripts/dq_reporting/summarise_submissions.R')

# compile excel report
compile_dq_report()


