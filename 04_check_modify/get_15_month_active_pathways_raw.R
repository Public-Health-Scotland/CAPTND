#### written by Alan Coventry, November 2026
# uses activity_cols to identify columns which had activity within a 15 month period.. 
#from that it, determines which pathways should be kept,even if beyond last 15 months

library(dplyr)
library(lubridate)

source("04_check_modify/remove_spaces_data_keys.R")
source("04_check_modify/pad_chi.R")
source("02_setup/add_patient_id.R")

### call with:
# captnd_15_months <- get_15_month_active_pathways_raw(
#   df_swift_clean,
#   end_date = "2026-09-30"
# )

## source it with source("04_check_modifyp/get_15_month_active_pathways_raw.R")

get_15_month_active_pathways_raw <- function(df, end_date) {
  
  ##### 1) Select raw SWIFT data ####
  raw_data <- df
  
  ##### 2) Set 15-month window #####
  end_date <- as.Date(end_date)
  start_date <- end_date %m-% months(15)
  
  #### 3) Identify clinical activity dates ####
  activity_cols <- c(
    "ref_date",
    "ref_rec_date",
    "ref_rej_date",
    "app_date",
    "cancellation_date",
    "act_code_sent_date",
    "treat_start_date",
    "case_closed_date",
    "unav_date_start",
    "unav_date_end"
  )
  
  activity_cols <- intersect(activity_cols, names(raw_data))
  
  #### 4) Create pathway identifiers using CAPTND functions ####
  raw_data <- raw_data %>%
    remove_spaces_data_keys() %>%
    pad_chi() %>%
    add_patient_id()
  
  ##### 5) Find pathways with activity in 15-month period ####
  recent_pathways <- raw_data %>%
    filter(
      if_any(
        all_of(activity_cols),
        ~ !is.na(.x) &
          as.Date(.x) >= start_date &
          as.Date(.x) <= end_date
      )
    ) %>%
    distinct(across(all_of(data_keys)))
  
  ##### 6) Retrieve ALL historical records for these pathways #####
  captnd_raw_15_months <- raw_data %>%
    semi_join(
      recent_pathways,
      by = data_keys
    )
  
  ##### 7) Display results #####
  cat(
    "Period:",
    as.character(start_date),
    "to",
    as.character(end_date),
    "\n"
  )
  
  cat("Pathways:", nrow(recent_pathways), "\n")
  cat("Historical records:", nrow(captnd_raw_15_months), "\n")
  
  ##### 8) Return data #####
  return(captnd_raw_15_months)
}