
########################################################.
### Controller script for Consolidating Globalscape  ###
########################################################.

#This script makes connection to database, loads SWIFT data, merges CAMHS and PT,
#renames columns.

# 1 - Housekeeping --------------------------------------------------------
# 1.1 - Load packages -----------------------------------------------------

library(odbc)
library(rstudioapi)
library(plyr)
library(dbplyr)
library(dplyr)

# 1.2 Source column renamer function ------------------------------------------
source('./functions/swift_column_renamer.R')
source('functions/globalscape_column_renamer.R')
source('functions/globalscape_data_loader.R')

# 1.3 - Establish database connection -------------------------------------

con <- dbConnect(odbc(),
                 dsn = "CAPTND",
                 uid = askForPassword("Enter network username:"), 
                 pwd = askForPassword("Enter network password:"))
