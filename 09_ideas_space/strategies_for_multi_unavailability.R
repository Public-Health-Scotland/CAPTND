### multiple unavailabiltiy periods options 

# flag strategy - create label for unavailabiltiy periods 1, 2, 3, etc than use those for group_by (?)
# row-wise strategy - a row for each unav period against each sub month (like happens in lothian)
# column-wise streatgy - make start and end date lag columns so we get a wider df with columns for each unav period

test <- filter(df, ucpn == "136002417048M") 

# flag stretagy
test1 <- test |> 
  mutate(unav_no = factor(unav_date_start, labels=seq(unique("unav_date_start")))) |> 
  ungroup() |> 
  group_by(!!!syms(data_keys), unav_no) 


# row-wise strategy


# column-wise strategy
test3 <- test |> 
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
                                    TRUE ~ unav_end_lag_2)) # 2x lags would allow for up to 3 unavailabiltiy periods - enough?





# make test data for multiple unavailability issue
df1 <- df %>% 
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o))) |> 
  group_by(!!!syms(data_keys)) %>%
  mutate(unav_no = factor(unav_date_start, labels=seq(unique("unav_date_start")))) |> 
  filter(any(unav_no == "12" | unav_no == "13"))
  

#136002113906J lanark x3
#107001022749K lothian x3
#107001469098C lothian no treat start x3
#1070010262036 lothian x2
#1360023866090 lanark x2

df_simple <- df %>% 
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o, header_date_o))) |> 
  filter(ucpn == "136002113906J" | ucpn == "107001022749K" | ucpn == "107001469098C" 
         | ucpn == "1070010262036" | ucpn == "1360023866090") |> 
  arrange(ucpn, app_date) |> 
  save_as_parquet("../../../data/multi_unavailability_test_data")

