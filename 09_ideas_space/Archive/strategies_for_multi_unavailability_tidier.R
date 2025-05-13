### multiple unavailabiltiy periods strategy tidied and fcased - need to compare performance against untidy version 

# flag strategy - create label for unavailabiltiy periods 1, 2, 3, etc than use those for group_by (?)
# row-wise strategy - a row for each unav period against each sub month (like happens in lothian)
# column-wise streatgy - make start and end date lag columns so we get a wider df with columns for each unav period
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

test <- filter(df, ucpn == "136002417048M") 


test2 <- df %>% 
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o, header_date_o))) |> 
  filter(ucpn == "136002113906J" | ucpn == "107001022749K" | ucpn == "107001469098C" 
         | ucpn == "1070010262036" | ucpn == "1360023866090") |> 
  arrange(ucpn, app_date) |> 
  group_by(!!!syms(data_keys))



# column-wise strategy
test3 <- test |> 
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o))) |> 
  
  mutate(dna_date = if_else(#app_purpose %in% c(2, 3) & removing - should reset for treatment and assessment app d/cna/w
    att_status %in% c(3, 5, 8), #&    app_date <= first_treat_app
    app_date, NA_Date_)) |> # makes a column with dates for any D/CNA/W # will need to add cancellation date here
  
  fill(c("unav_date_start", "unav_date_end"), .direction = "downup") |> # Get unavailability in every row (what about multiple unavailability??)
  
  filter(!is.na(dna_date)) |> # removes gaps between dnas so lag doesn't get interrupted
  
  mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
         
         dna_lag = case_when(
           is.na(dna_date) &
             !is.na(dna_lag) ~ NA_Date_,
           TRUE ~ dna_lag), # removes hanging lag date
         
         dna_lag = if_else(dna_date == first(dna_date), 
                           ref_rec_date_opti, dna_lag),  # adds ref date as 'first' lag date 
         
         dna_interval = as.integer(dna_date - dna_lag)) |> 
  
 # Get multiple unavailability in every row 
  
  mutate(unav_start_lag = lag(unav_date_start, n=1),
         unav_end_lag = lag(unav_date_end, n=1),
         unav_start_lag_2 = lag(unav_start_lag, n=1),
         unav_end_lag_2 = lag(unav_end_lag, n=1), # 2x lags allows for up to 3 unavailabilty periods - enough for data as of 12/2024
         #Need to do a fill??
         
         unav_start_lag_2 = case_when(unav_start_lag_2 == unav_date_start | unav_start_lag_2 == unav_start_lag ~ NA_Date_, # if lag date 2 doesnt match either lag 1 or original start date, keep it
                                      TRUE ~ unav_start_lag_2),
         unav_end_lag_2 = case_when(unav_end_lag_2 == unav_date_end | unav_end_lag_2 == unav_end_lag ~ NA_Date_, # if lag date 2 doesnt match either lag 1 or original end date, keep it
                                    TRUE ~ unav_end_lag_2),
         unav_start_lag = case_when(unav_start_lag == unav_date_start | unav_start_lag == unav_start_lag_2 ~ NA_Date_, # if lag date 1 doesnt match either lag 2 or original start date, keep it
                                    TRUE ~ unav_start_lag),
         unav_end_lag = case_when(unav_end_lag == unav_date_end | unav_end_lag == unav_end_lag_2 ~ NA_Date_, # if lag date 1 doesnt match either lag 1 or original end date, keep it
                                  TRUE ~ unav_end_lag)) 

  start_vec <- c("unav_date_start", "unav_start_lag", "unav_start_lag_2")
  end_vec <- c("unav_date_end", "unav_end_lag", "unav_end_lag_2")

test1 <- test3 |>
  mutate(across(all_of(start_vec), ~fcase(. > dna_lag & . < dna_date, ., # if start date is within the dna period, keep it
                                          . < dna_lag, dna_lag, # if the unavailability starts before the dna lag (start of period), use the lag date as unav start
                                          default = NA_Date_)),
         across(all_of(end_vec), ~fcase(. > dna_lag & . < dna_date, ., # if the unavailability end date is within the dna period, keep it
                                        . > dna_date, dna_date, # if the unavailability end date is after the dna date (end of period), use the dna date as the terminus
                                        default = NA_Date_)),

         unav_date_start = fcase(!is.na(unav_date_start) & !is.na(unav_date_end), unav_date_start, #only keep start date if we have both dates
                                     default = NA_Date_),
         unav_date_end = fcase(!is.na(unav_date_end) & !is.na(unav_date_start), unav_date_end, # only keep end date if we have both dates
                                   default = NA_Date_),
         
         unav_start_lag = fcase(!is.na(unav_start_lag) & !is.na(unav_end_lag), unav_start_lag, #only keep start date if we have both dates
                                 default = NA_Date_),
         unav_end_lag = fcase(!is.na(unav_end_lag) & !is.na(unav_start_lag), unav_end_lag, # only keep end date if we have both dates
                               default = NA_Date_),
    
         unav_start_lag_2 = fcase(!is.na(unav_start_lag_2) & !is.na(unav_end_lag_2), unav_start_lag_2, #only keep start date if we have both dates
                                default = NA_Date_),
         unav_end_lag_2 = fcase(!is.na(unav_end_lag_2) & !is.na(unav_start_lag_2), unav_end_lag_2, # only keep end date if we have both dates
                              default = NA_Date_)) |> 
  
  #add up unavailability periods applicable to each dna period
  mutate(valid_unav = as.numeric(unav_date_end - unav_date_start),
         valid_unav_lag = as.numeric(unav_end_lag - unav_start_lag),
         valid_unav_lag2 = as.numeric(unav_end_lag_2 - unav_start_lag_2)) |> 
  
  mutate_at(c('valid_unav','valid_unav_lag', 'valid_unav_lag2'), ~replace_na(.,0)) |> 
  mutate(unav_period_dna = valid_unav + valid_unav_lag + valid_unav_lag2) |> 



# calculate the unavailability period
         
  mutate(dna_interval_opti = dna_interval - unav_period_dna, # calculate the dna interval with any valid unavailability subtracted
         
         dna_interval_opti = case_when(is.na(dna_interval_opti) & !is.na(dna_interval) ~ dna_interval,
                                       TRUE ~ dna_interval_opti)) |> 
  
  filter(cumall(!dna_interval_opti > 126)) |>  # keeps records UP TO the first instance where the interval exceeds 126 days #HOW DOES THIS WORK IF HAVE MULTIPEL ROWS WITH SAME DNA BUT DIFFERENT UNAVS - AS IN TEST2
  
  mutate(clock_start = max(dna_date, na.rm = TRUE)) #|>  # make clock_start date be the max remaining dna date
  # 
  # select(all_of(data_keys), app_date, clock_start) |> # selects relevant columns
  # distinct() 





