
##################################################.
### CAPTND: Load and join old Globalscape data ###
##################################################.

# NB this script only works for charlie and maria (joana does not have access 
# to all staging areas)

# 1 - Housekeeping --------------------------------------------------------

# 1.1 - Load packages -----------------------------------------------------

library(odbc)
library(rstudioapi)
library(plyr)
library(dbplyr)
library(dplyr)
library(purrr)


# 1.2 - Source functions --------------------------------------------------

source("./scripts/functions/globalscape_data_loader.R")
source("./scripts/functions/globalscape_stack_stages.R")
source("./scripts/functions/globalscape_stack_new_return_apps.R")
source("./scripts/functions/globalscape_column_renamer.R")



# 1.3 - Establish database connection -------------------------------------

con <- dbConnect(odbc(),
                 dsn = "CAPTND",
                 uid = askForPassword("Enter network username:"),
                 pwd = askForPassword("Enter network password:"))


# 2 - Load data -----------------------------------------------------------

captnd_all <- load_globalscape_data(con)


# 3 - Join data -----------------------------------------------------------

# 3.1 - Stack stages ------------------------------------------------------

captnd_all <- stack_stages(captnd_all)
# captdn_all_dq <- captnd_all # better to store this in case better for DQ checks later?


# 3.2 - Stack new and return appointments only ----------------------------

captnd_all <- stack_new_return_apps(captnd_all)


# 4 - Set column names ----------------------------------------------------

captnd_all <- globalscape_column_renamer(captnd_all)

#saveRDS(captnd_all, "./problems/shorewise_glob_")

# 5 - Clean joining vars -------------------------------------------------

# TBC...
# UCPN 
# - replace "NULL" with NA
# - 


# 6 - Merge all to data frame ---------------------------------------------


glob_df <- captnd_all %>% 
  reduce(full_join, by = c('ucpn', # CLEAN, create stand-in UCPN if missing?
                           'upi', # DROP?
                           'chi', 
                           'hb_name', 
                           'dataset_type', 
                           'sub_source')) %>% 
  arrange(ucpn)



# To do:
# - Clean data required for join? UCPN?
# - structure as we want it? Can't join by FILE_ID as does not exist in Globalscape






