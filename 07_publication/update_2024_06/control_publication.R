
###############################################.
### Generate figures for CAPTND publication ###
###############################################.

# Author: Charlie Smith
# Date: 2023-11-14

# Note: recreate publication figures and charts using 'new' CAPTND (shorewise) data


# Step 1: Enter last month of data to include in publication --------------

month_end <- "2024-05-01"


# Step 2 - Run these scripts in sequence ----------------------------------

source("./07_publication/update_2024_06/chapters/1_load_packages.R")
source("./07_publication/update_2024_06/chapters/2_load_functions.R")
source("./07_publication/update_2024_06/chapters/3_set_constants.R")


# 3 - Analyse Data --------------------------------------------------------

summarise_referrals_by_ref_source()

summarise_non_acceptance()
summarise_non_acceptance_reason()
summarise_non_acceptance_action()


# 4 - Compile excel workbooks ---------------------------------------------

# part of measure scoping
compile_referrals_by_ref_source()

compile_non_acceptance_summary()
compile_non_acceptance_reason_summary()
compile_non_acceptance_action_summary()



# 5 - Create tables for publication ---------------------------------------

create_table_acceptance_rate()




