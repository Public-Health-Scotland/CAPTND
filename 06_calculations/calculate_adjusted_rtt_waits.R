
#################################################################.
### Calculate adjusted referral to treatment waiting times v2 ###
#################################################################.

# Authors: Bex Madden and Charlie Smith
# Date: 2024-05-20
# Updated: 2024-02-03 by Bex Madden

# translation of this function into plain english can be found in /MentalHealth5/CAPTND/background_info/RTT_adjustment_calculation_translated.docx


calculate_adjusted_rtt_waits <- function(df, include_QA = c(TRUE, FALSE)){
  
  df_rtt <- df |>
    group_by(!!!syms(data_keys)) |> # for each pathway...
    arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, app_date_o))) |>
    
    filter((!is.na(first_treat_app) | 
              any(!is.na(act_code_sent_date))) & # filter for records with treatment start and accepted referrals 
             ref_acc_opti == "1") |> 
    
    # calculate basic unadjusted RTT
    mutate(rtt_unadj = as.integer(case_when(
      !is.na(act_code_sent_date) ~ act_code_sent_date - ref_rec_date_opti,
      TRUE ~ first_treat_app - ref_rec_date_opti)), # make treatment start column only for code sent date >= first treat app?
    
      # uses unav_days_no to fill unav start/end date if one is missing
      unav_date_start = case_when(
        is.na(unav_date_start) &
          !is.na(unav_date_end) &
          !is.na(unav_days_no) ~ unav_date_end - unav_days_no,
        TRUE ~ unav_date_start),
      
      unav_date_end = case_when(
        is.na(unav_date_end) &
          !is.na(unav_date_start) &
          !is.na(unav_days_no) ~ unav_date_start + unav_days_no,
        TRUE ~ unav_date_end)) |> 
    
    # select relevant columns
    select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                     app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                     unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                     act_code_sent_date_o)), rtt_unadj) # save out here?
  
  message('DF ready, calculating clock reset\n')
  
  # DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
  df_dna <- df_rtt |>
    
    mutate(dna_date = if_else( # should reset for treatment or assessment app d/cna/w
      att_status %in% c(3, 5, 8) &
        app_date <= first_treat_app,
      app_date, NA_Date_)) |> # makes a column with dates for any D/CNA/W # will need to add cancellation date here
    
    fill(c("unav_date_start", "unav_date_end"), .direction = "downup") |> # Get any unavailability filled in every row of pathway
    
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
                                    TRUE ~ unav_end_lag)) #save out here?
  
  start_vec <- c("unav_date_start", "unav_start_lag", "unav_start_lag_2")
  end_vec <- c("unav_date_end", "unav_end_lag", "unav_end_lag_2")
  
  df_reset <- df_dna |>
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
    
    # calculate the clock reset
    
    mutate(dna_interval_opti = dna_interval - unav_period_dna, # calculate the dna interval with any valid unavailability subtracted
           
           dna_interval_opti = case_when(is.na(dna_interval_opti) & !is.na(dna_interval) ~ dna_interval,
                                         TRUE ~ dna_interval_opti)) |> 
    
    filter(cumall(!dna_interval_opti > 126)) |>  # keeps records UP TO the first instance where the interval exceeds 126 days #HOW DOES THIS WORK IF HAVE MULTIPEL ROWS WITH SAME DNA BUT DIFFERENT UNAVS - AS IN TEST2
    
    mutate(clock_start = max(dna_date, na.rm = TRUE)) |>  # make clock_start date be the max remaining dna date
    
    select(all_of(data_keys), app_date, clock_start) |> # selects relevant columns
    distinct() # removes duplicates ## save out here?
  
  message('Clock reset completed, calculating pauses\n')
  
  # unavailability logic - pausing the clock for unavailability before 18 weeks (or after for PT past 01/04/2024)
  
  df_rtt_complete <- df_rtt |>
    
    left_join(df_reset, by = c(all_of(data_keys), "app_date")) |> # appends new clock start date to complete data
    
    fill(c("clock_start"), .direction = "downup") |> 
    
    mutate(clock_start = case_when(is.na(clock_start) ~ ref_rec_date_opti,
                                   TRUE ~ clock_start), # for pathways without dnas, uses ref_rec_date as clock_start
           
           guarantee_date = clock_start + 126, # make new guarantee date relative to the clock_start
           
           unav_date_start = case_when(clock_start > unav_date_start &
                                         clock_start < unav_date_end ~ clock_start,
                                       TRUE ~ unav_date_start), # if the clock start date is in the middle of an unavailability period, use it as the start of the unavailability period
           
           unav_period = as.integer(unav_date_end - unav_date_start + 1), # calculate difference between unav start and end dates
           
           time_to_first_treat_app = case_when(
             !is.na(act_code_sent_date) ~ as.integer(act_code_sent_date - clock_start),
             TRUE ~ as.integer(first_treat_app - clock_start)), # use act code sent date if its there, or first treat app if it isnt
           
           unav_period_opti = fcase(
             
             dataset_type == "PT" &
               !is.na(unav_date_start) &
               !is.na(unav_date_end) &
               unav_date_start >= clock_start & 
               unav_date_start <= guarantee_date &
               unav_date_start < first_treat_app & # this is for PT unavailability before 18 weeks before 1st apr
               unav_date_start < "2024-04-01", unav_period,
             
             dataset_type == "PT" &
               !is.na(unav_date_start) &
               !is.na(unav_date_end) &
               unav_date_start >= clock_start &
               unav_date_start < first_treat_app & # this is for PT unavailability after 1st apr
               unav_date_start >= "2024-04-01", unav_period,
             
             dataset_type == "CAMHS" &
               !is.na(unav_date_start) &
               !is.na(unav_date_end) &
               unav_date_start >= clock_start &
               unav_date_start <= guarantee_date & # this is for CAMHS unavailability
               unav_date_start < first_treat_app, unav_period,
             
             default = NA_integer_
           )) |>
    
    # select relevant variables
    select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, ref_rec_date_opti_o,
                     unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                     first_treat_app_o, act_code_sent_date_o)), 
           clock_start, unav_period_opti, time_to_first_treat_app, rtt_unadj)  |> 

    
    distinct() |> # keeps unique rows so we don't artifically sum same period up
    
    mutate(unav_opti_total = sum(unav_period_opti, na.rm = TRUE), 
           
           rtt_adj = time_to_first_treat_app - unav_opti_total,
           
           rtt_adj = case_when(rtt_adj < 0 ~ NA_integer_,
                               TRUE ~ rtt_adj),
           
           rtt_unadj = case_when(rtt_unadj < 0 ~ NA_integer_,
                                 TRUE ~ rtt_unadj)) |> 
    
    lazy_dt() |> 
    slice(1) |> # return one row per pathway #save out here?
    ungroup() |> 
    as.data.frame()
  
  message('RTT adjustment completed!\n')
  
  
  ## QA bits
  if(include_QA == TRUE){
    # flag open-ended unav
    df_open <- df_rtt |>
      mutate(unav_days_no = as.character(unav_days_no),
             is_unav_open_ended = case_when(
               is.na(unav_date_start) &
                 !is.na(unav_date_end) &
                 is.na(unav_days_no) ~ TRUE,
               
               is.na(unav_date_end) &
                 !is.na(unav_date_start) &
                 is.na(unav_days_no) ~ TRUE,
               
               TRUE ~ FALSE)) |>
      filter(is_unav_open_ended == TRUE) |>
      save_as_parquet(paste0(rtt_dir, "/flag_open_ended_unavailability"))
    
    
    # flag app date without unavailability end date
    df_end <- df_rtt |>
      mutate(unav_days_no = as.character(unav_days_no),
             is_unav_end_app_date = case_when(
               is.na(unav_date_end) &
                 is.na(unav_days_no) &
                 !is.na(unav_date_start) &
                 app_date > unav_date_start ~ TRUE,
               
               TRUE ~ FALSE)) |>
      filter(is_unav_end_app_date == TRUE) |>
      save_as_parquet(paste0(rtt_dir, "/flag_app_date_unav_end"))
    
    
    # flag ref date after first treat app aka negative wait
    df_neg <- df_rtt |>
      mutate(is_negative_wait = case_when(
        first_treat_app < ref_rec_date_opti ~ TRUE,
        TRUE ~ FALSE)) |>
      filter(is_negative_wait == TRUE) |> 
      slice(1) |> 
      save_as_parquet(paste0(rtt_dir, "/flag_negative_waits"))
  } else {}
  
  
  return(df_rtt_complete)
  
}