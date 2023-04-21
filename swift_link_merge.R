
########################################.
### CAPTND: Load and join SWIFT data ###
########################################.

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
source('scripts/functions/swift_column_renamer.R')

# 1.3 - Establish database connection -------------------------------------

con <- dbConnect(odbc(),
                 dsn = "CAPTND",
                 uid = askForPassword("Enter network username:"), 
                 pwd = askForPassword("Enter network password:"))


# 2 - Load data from staging area and join CAMHS and PT sheets -----------------
# 2.1 - Load SWIFT data and change column names ---------------------------------------------------

# SWIFT CAMHS
swift_camhs <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_CAMHS"))) %>% 
  rename_swift_columns(.)
 
# SWIFT PT
swift_pt <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_PT"))) %>% 
  rename_swift_columns()
  

# 2.2 -  Join CAMHS and PT -----------------------------------------------------
swift_all <- rbind.fill(swift_camhs, swift_pt) 


# checks

# ucpn is not unique for every combination of dataset and hb name
check <- swift_all %>% 
  select(ucpn, hb_name, dataset_type, chi) %>% 
  distinct() %>% 
  group_by(ucpn, hb_name, dataset_type) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

peep <- swift_all %>% filter(ucpn == "1010185749721") # good example ^

export(peep, "./problems/peep_not_anon.csv", format = "csv")

swift_test_ds <- swift_all %>% 
  ungroup() %>% 
  filter(ucpn == "0") %>% 
  select(chi, ucpn, hb_name, dataset_type) %>% 
  distinct() %>% 
  head(5)

swift_test_output <- swift_test_ds %>% 
  group_by(ucpn, hb_name) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

