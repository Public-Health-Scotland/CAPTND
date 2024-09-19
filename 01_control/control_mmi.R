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
source("./07_publication/script/functions/summarise_appointments_att.R")
source("./07_publication/script/functions/summarise_patients_seen.R")
source("./07_publication/script/functions/get_appointments_df.R")
source("./07_publication/script/functions/add_sex_description.R")
source("./07_publication/script/functions/create_plots_patients_seen.R")
source("./02_setup/save_df_as_parquet.R")

# constants?
month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# Step 3 - Analyse Data ---------------------------------------------------

summarise_appointments_att() # key output "/appointments_att/apps_att_mth_hb.parquet"
summarise_patients_seen() # key outputs "/patients_seen/pat_seen_unadj_wait_grp_mth.parquet' and "/patients_seen/pat_seen_adj_wait_grp_mth.parquet'

# Step 4 - Create plots ---------------------------------------------------

create_plots_patients_seen("PT")
create_plots_patients_seen("CAMHS")

