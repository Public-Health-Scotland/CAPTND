
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

# 1.2 Source functions --------------------------------------
source('config/new_colnames.R')
source('setup/open_last_parquet_with_rrt_eval.R')








