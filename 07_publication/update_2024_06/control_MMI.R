###########################.
### Generate CAPTND MMI ###
###########################.

# Author: Charlie Smith
# Date: 2024-09-18



# Step 1: Enter last month of data to include in MMI ----------------------

month_end <- "2024-08-01"


# Step 2 - Run these scripts in sequence ----------------------------------
# packages?

# functions?
source("./07_publication/update_2024_06/functions/summarise_appointments_att.R")
source("./07_publication/update_2024_06/functions/get_appointments_df.R")
source("./07_publication/update_2024_06/functions/add_sex_description.R")
source("./02_setup/save_df_as_parquet.R")

# constants?
month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# Step 3 - Analyse Data ---------------------------------------------------

summarise_appointments_att()

# Step 4 - Create formatted data tables for MMI ---------------------------

create_mmi_table_app_att()


