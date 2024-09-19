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
source("./07_publication/script/functions/summarise_appointments_att.R")
source("./07_publication/script/functions/get_appointments_df.R")
source("./07_publication/script/functions/add_sex_description.R")
source("./02_setup/save_df_as_parquet.R")
source("./07_publication/script/functions/tidy_age_group_order.R")

# constants?  (separate script)
month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# Step 3 - Analyse Data ---------------------------------------------------

summarise_appointments_att() # key output will be "/appointments_att/apps_att_mth_hb.parquet"




