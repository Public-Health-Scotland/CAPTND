
###########################################.
### Controller script for calculations  ###
###########################################.

#This takes the last rtt evaluated file and calculates different measure from it


# 1.1 - Load packages -----------------------------------------------------

# library(dplyr)
# library(arrow)
# library(purrr)
# library(stringr)
# library(lubridate)
# library(plyr)
# library(conflicted)
# conflict_prefer('filter','dplyr')
# conflict_prefer('mutate','dplyr')
# conflict_prefer('summarise', 'dplyr')

# 1.2 - Source functions -------------------------------------------------
source('calculations/calculate_open_cases.R')
source('calculations/calculate_patients_waiting.R')
source('calculations/calculate_patients_seen.R')
source('calculations/calculate_referrals.R')
source('calculations/calculate_appointments.R')
source('calculations/calculate_attendance_status_rates.R')
source('calculations/calculate_first_contact.R')
source('data_quality/product1.R')
source('data_quality/product2.R')


# 2 - open most recent RTT eval file--------------------------------------

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))


# 2.1 Calculate variables -------------------------------------------------

most_recent_month_in_data=df %>% 
  select(!!header_date_o) %>% 
  distinct() %>% 
  pull() %>% 
  max() %>% 
  ceiling_date(unit = 'month')-1

calculate_open_cases(df, most_recent_month_in_data)
calculate_patients_waiting(df, most_recent_month_in_data) 
calculate_patients_seen(df)
calculate_referrals(df, most_recent_month_in_data)
calculate_appointments(df)
calculate_attendance_status_rates(df)
calculate_first_contact(df)


# 2.2 Produce reports -----------------------------------------------------


id_app_after_case_closed(df)

make_product_1()
make_product_2(df)

