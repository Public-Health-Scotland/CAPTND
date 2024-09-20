###########################.
### Generate CAPTND MMI ###
###########################.

# Author: Charlie Smith
# Date: 2024-09-18



# Step 1: Enter last month of data to include in MMI ----------------------

month_end <- "2024-08-01"


# Step 2 - Run these scripts in sequence ----------------------------------
# packages?

# functions? (separate script)
source("./07_publication/script/chapters/2_load_functions.R")

# constants?  (separate script)
source("./07_publication/script/chapters/3_set_constants.R")

# Step 3 - Analyse Data ---------------------------------------------------

summarise_appointments_att() # key output will be "/appointments_att/apps_att_mth_hb.parquet"




