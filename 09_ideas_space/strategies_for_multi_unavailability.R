### multiple unavailabiltiy periods options 

# flag strategy - create label for unavailabiltiy periods 1, 2, 3, etc than use those for group_by (?)
# row-wise strategy - a row for each unav period against each sub month (like happens in lothian)
# column-wise streatgy - make start and end date lag columns so we get a wider df with columns for each unav period
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

test <- filter(df, ucpn == "136002417048M") 

# flag stretagy
test1 <- test |> 
  mutate(unav_no = factor(unav_date_start, labels=seq(unique("unav_date_start")))) |> 
  ungroup() |> 
  group_by(!!!syms(data_keys), unav_no) 


# row-wise strategy


# column-wise strategy
test3 <- test |> 
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o))) |> 
  
  fill(c("unav_date_start", "unav_date_end"), .direction = "downup") |> # Get unavailability in every row (what about multiple unavailability??)
  
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
                                    TRUE ~ unav_end_lag_2)) |>  # 2x lags would allow for up to 3 unavailabiltiy periods - enough?

  mutate(dna_date = if_else(#app_purpose %in% c(2, 3) & removing - should reset for treatment and assessment app d/cna/w
    att_status %in% c(3, 5, 8),# &
     # app_date <= first_treat_app, 
    app_date, NA_Date_)) |> # makes a column with dates for any D/CNA/W # will need to add cancellation date here
  
  #fill(c("unav_date_start", "unav_date_end"), .direction = "downup") |> # Get unavailability in every row (what about multiple unavailability??)
  
  filter(!is.na(dna_date)) |> # removes gaps between dnas so lag doesn't get interrupted
  
  mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
         
         dna_lag = case_when(
           is.na(dna_date) &
             !is.na(dna_lag) ~ NA_Date_,
           TRUE ~ dna_lag), # removes hanging lag date
         
         dna_lag = if_else(dna_date == first(dna_date), 
                           ref_rec_date_opti, dna_lag),  # adds ref date as 'first' lag date 
         
         dna_interval = as.integer(dna_date - dna_lag)) |>  # calculates difference between one dna date and the previous dna date
  
  # NEED TO ACCOUNT FOR UNAVAILABILITY IN INTERVALS     
  mutate(unav_date_start = case_when(unav_date_start < dna_date ~ unav_date_start, # keep unavailbility start date if before the dna date for that row
                                     TRUE ~ NA_Date_),
         unav_date_end = case_when(!is.na(unav_date_start) & # if the unavailability end date is after the dna date, use the dna date as the terminus
                                     unav_date_end > dna_date ~ dna_date,
                                   TRUE ~ unav_date_end),
         unav_date_end = case_when(unav_date_end < dna_lag ~ NA_Date_,
                                   TRUE ~ unav_date_end),
         unav_date_end = case_when(unav_date_end <= dna_date ~ unav_date_end, # keep unavailbility end date if it is before or equal to dna date
                                   TRUE ~ NA_Date_),
         unav_date_start = case_when(unav_date_start < dna_lag ~ dna_lag, # if the dna start date is after the previous dna date (when multiple dnas), use the previous dna date as the start date
                                     TRUE ~ unav_date_start),
         unav_date_start = case_when(!is.na(unav_date_start) & is.na(unav_date_end) ~ NA_Date_,
                                     TRUE ~ unav_date_start),
         unav_date_end = case_when(!is.na(unav_date_end) & is.na(unav_date_start) ~ NA_Date_,
                                   TRUE ~ unav_date_end)) |> 
  
  mutate(unav_start_lag = case_when(unav_start_lag < dna_date ~ unav_start_lag, # keep unavailbility start date if before the dna date for that row
                                     TRUE ~ NA_Date_),
         unav_end_lag = case_when(!is.na(unav_start_lag) & # if the unavailability end date is after the dna date, use the dna date as the terminus
                                    unav_end_lag > dna_date ~ dna_date,
                                   TRUE ~ unav_end_lag),
         unav_end_lag = case_when(unav_end_lag < dna_lag ~ NA_Date_,
                                   TRUE ~ unav_end_lag),
         unav_end_lag = case_when(unav_end_lag <= dna_date ~ unav_end_lag, # keep unavailbility end date if it is before or equal to dna date
                                   TRUE ~ NA_Date_),
         unav_start_lag = case_when(unav_start_lag < dna_lag ~ dna_lag, # if the dna start date is after the previous dna date (when multiple dnas), use the previous dna date as the start date
                                     TRUE ~ unav_start_lag),
         unav_start_lag = case_when(!is.na(unav_start_lag) & is.na(unav_end_lag) ~ NA_Date_,
                                     TRUE ~ unav_start_lag),
         unav_end_lag = case_when(!is.na(unav_end_lag) & is.na(unav_start_lag) ~ NA_Date_,
                                   TRUE ~ unav_end_lag)) |> 
  
  mutate(unav_start_lag_2 = case_when(unav_start_lag_2 < dna_date ~ unav_start_lag_2, # keep unavailbility start date if before the dna date for that row
                                    TRUE ~ NA_Date_),
         unav_end_lag_2 = case_when(!is.na(unav_start_lag_2) & # if the unavailability end date is after the dna date, use the dna date as the terminus
                                      unav_end_lag_2 > dna_date ~ dna_date,
                                   TRUE ~ unav_end_lag_2),
         unav_end_lag_2 = case_when(unav_end_lag_2 < dna_lag ~ NA_Date_,
                                   TRUE ~ unav_end_lag_2),
         unav_end_lag_2 = case_when(unav_end_lag_2 <= dna_date ~ unav_end_lag_2, # keep unavailbility end date if it is before or equal to dna date
                                   TRUE ~ NA_Date_),
         unav_start_lag_2 = case_when(unav_start_lag_2 < dna_lag ~ dna_lag, # if the dna start date is after the previous dna date (when multiple dnas), use the previous dna date as the start date
                                    TRUE ~ unav_start_lag_2),
         unav_start_lag_2 = case_when(!is.na(unav_start_lag_2) & is.na(unav_end_lag_2) ~ NA_Date_,
                                    TRUE ~ unav_start_lag_2),
         unav_end_lag_2 = case_when(!is.na(unav_end_lag_2) & is.na(unav_start_lag_2) ~ NA_Date_,
                                   TRUE ~ unav_end_lag_2))





# calculate the unavailbility period
         
         dna_interval_opti = dna_interval - unav_period_dna, # calculate the dna interval with any valid unavailability subtracted
         
         dna_interval_opti = case_when(is.na(dna_interval_opti) & !is.na(dna_interval) ~ dna_interval,
                                       TRUE ~ dna_interval_opti)) |> 
  
  filter(cumall(!dna_interval_opti > 126)) #|>  # keeps records UP TO the first instance where the interval exceeds 126 days
  
  # mutate(clock_start = max(dna_date, na.rm = TRUE)) |>  # make clock_start date be the max remaining dna date
  # 
  # select(all_of(data_keys), app_date, clock_start) |> # selects relevant columns
  # distinct() 




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

df_rtt <- read_parquet("../../../data/multi_unavailability_test_data.parquet")

