
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
source('./setup/swift_column_renamer.R')
source('./setup/save_df_as_parquet.R')

# 1.3 - Establish database connection -------------------------------------

con <- dbConnect(odbc(),
                 dsn = "CAPTND",
                 uid = askForPassword("Enter network username:"), 
                 pwd = askForPassword("Enter network password:"))


# 2 - Load data from staging area and join CAMHS and PT sheets -----------------
# 2.1 - Load SWIFT data and change column names ---------------------------------------------------

# SWIFT CAMHS
swift_camhs <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_CAMHS"))) %>% 
  rename_swift_columns()
 
# SWIFT PT
swift_pt <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_PT"))) %>% 
  rename_swift_columns()
  

# 2.2 -  Join CAMHS and PT -----------------------------------------------------
swift_all <- rbind.fill(swift_camhs, swift_pt) 


# 3 Save as parquet -------------------------------------------------------
dir.create(paste0("../../../output/swift_extract_", Sys.Date()))
save_as_parquet(swift_all, paste0("../../../output/swift_extract_", Sys.Date(),"/swift_extract"))


