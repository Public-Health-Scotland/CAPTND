
############################################.
### RTT logic - unavailability wrangling ###
############################################.

# Author: Charlie Smith
# Date: 2024-11-11

# Objective:






df_test <- read_parquet("../../../data/multi_unavailability_test_data.parquet") |> 
  select(c(header_date, dataset_type, hb_name, patient_id, ucpn, ref_rec_date_opti,
           app_date, act_code_sent_date, first_treat_app, unav_date_start, 
           unav_date_end, unav_days_no, app_purpose, att_status)) |> 
  
  filter(ucpn == "107001022749K") |> 
  
  mutate(unav_start_lag = lag(unav_date_start, n=1),
         unav_start_lag = case_when(!is.na(unav_start_lag) & is.na(unav_date_start) ~ NA_Date_,
                                    TRUE ~ unav_start_lag),
         unav_end_lag = lag(unav_date_end, n=1),
         unav_end_lag = case_when(!is.na(unav_end_lag) & is.na(unav_date_start) ~ NA_Date_,
                                  TRUE ~ unav_end_lag),
         unav_start_lag_2 = lag(unav_start_lag, n=1),
         unav_start_lag_2 = case_when(!is.na(unav_start_lag_2) & is.na(unav_start_lag) ~ NA_Date_,
                                      TRUE ~ unav_start_lag_2),
         unav_end_lag_2 = lag(unav_end_lag, n=1),
         unav_end_lag_2 = case_when(!is.na(unav_end_lag_2) & is.na(unav_end_lag) ~ NA_Date_,
                                    TRUE ~ unav_end_lag_2)) 


