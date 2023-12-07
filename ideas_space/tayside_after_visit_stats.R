
######################################################.
### Info for Patrick Foley following Tayside visit ###
######################################################.

# Name: Charlie Smith
# Date: 2023-12-05


# Required info:
# 1 ~ number of open cases that have not been seen for a long time + last app date + data keys
# 2 ~ number started treatment + data keys

# 1 - load packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(arr)



# 2 - Load data -----------------------------------------------------------
# NB data is loaded from control_calculations.R



# 3 - Filter data as required ---------------------------------------------
df_tayside <- df |> 
  filter(hb_name == "NHS Tayside")

rm(df)



# 4 - Analyses ------------------------------------------------------------


# 4.1 - Open cases, long time since last app ------------------------------

df_open_long <- df_tayside |> 
  group_by(across(all_of(data_keys))) |> 
  filter(is_case_closed == FALSE) |> 
  mutate(last_app_date = max(app_date, na.rm = TRUE), .after = app_date) |> 
  filter(app_date == last_app_date) |> 
  mutate(days_since_last_app = difftime((floor_date(Sys.Date(), unit = "month")-1),
                                        app_date, 
                                        units = "days"), 
         years_since_last_app = round(as.numeric( days_since_last_app / 365.25), 1)) |> 
  filter(years_since_last_app >= 1) |> 
  select(dataset_type, ucpn, chi, upi, last_app_date, case_closed_date, days_since_last_app, years_since_last_app) |> 
  distinct()



# 4.2 - Started treatment + data keys -------------------------------------

df_started_treatment <- df_tayside |> 
  filter(!is.na(treat_start_date)) # not going to work as all NA
  
  




