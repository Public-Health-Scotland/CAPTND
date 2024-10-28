
#################################################################.
### Calculate adjusted referral to treatment waiting times v2 ###
#################################################################.

# Authors: Bex Madden and Charlie Smith
# Date: 2024-05-20


calculate_adjusted_rtt_waits <- function(df, include_QA = c(TRUE, FALSE)){
  
  #vector of date columns for easy referance
  date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
                 "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
                 "unav_date_start", "unav_date_end", "header_date")
  
  df_rtt <- df |>
    group_by(!!!syms(data_keys)) |> # for each pathway...
    #mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y"))) |>
    arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, app_date_o))) |>
    
    filter((!is.na(first_treat_app) | 
             !is.na(act_code_sent_date)) & # filter for records with treatment start and accepted referrals #OK TO FILTER?? ## ADD ANY()?
            ref_acc_last_reported == "1") |> 
    
    # calculate basic unadjusted RTT
    mutate(rtt_unadj = as.integer(case_when(
      !is.na(act_code_sent_date) ~ act_code_sent_date - ref_rec_date_opti,
      TRUE ~ first_treat_app - ref_rec_date_opti))) |> # make treatment start column only for code sent date >= first treat app?
    
    # select relevant columns
    select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                     app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                     unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                     act_code_sent_date_o)), rtt_unadj)
  
  message('DF ready, calculating clock reset\n')
  
  # DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
  df_reset <- df_rtt |>
    mutate(dna_date = if_else(#app_purpose %in% c(2, 3) & removing - should reset for treatment and assessment app d/cna/w
      att_status %in% c(3, 5, 8) &
        app_date < first_treat_app, # should this <= instead?
      app_date, NA_Date_)) |> # makes a column with dates for any D/CNA/W # will need to add cancellation date here
    
    filter(!is.na(dna_date)) |> # removes gaps between dnas so lag doesn't get interrupted
    
    mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
           
           dna_lag = case_when(
             is.na(dna_date) &
               !is.na(dna_lag) ~ NA_Date_,
             TRUE ~ dna_lag), # removes hanging lag date
           
           dna_lag = if_else(dna_date == first(dna_date), 
                             ref_rec_date_opti, dna_lag), # adds ref date as 'first' lag date 
           
           dna_interval = dna_date - dna_lag) |>    # calculates difference between one dna date and the previous dna date
    
    filter(cumall(!dna_interval > 126)) |>  # filters for records UP TO any instance where the interval exceeds 126 days
    
    mutate(clock_start = max(dna_date, na.rm = TRUE)) |>  # make clock_start date be the max remaining dna date
    
    select(all_of(data_keys), app_date, clock_start) |> # selects relevant columns
    distinct() # removes duplicates
  
  message('Clock reset completed, calculating pauses\n')
  
  # unavailability logic - pausing the clock for unavailability before 18 weeks (or after for PT past 01/04/2024)
  
  df_rtt_complete <- df_rtt |>
    
    left_join(df_reset, by = c(all_of(data_keys), "app_date")) |> # appends new clock start date to complete data
    
    fill(c("clock_start"), .direction = "downup") |> 
    
    mutate(clock_start = case_when(is.na(clock_start) ~ ref_rec_date_opti,
                                   TRUE ~ clock_start), # for pathways without dnas, uses ref_rec_date as clock_start
           
           guarantee_date = clock_start + 126, # make new guarantee date relative to the clock_start
           
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
             TRUE ~ unav_date_end),
           
           unav_date_start = case_when(clock_start > unav_date_start &
                                         clock_start < unav_date_end ~ clock_start,
                                       TRUE ~ unav_date_start), # if the clock start date is in the middle of an unavailability period, use it as the start of the unavailability period
           
           unav_period = as.integer(unav_date_end - unav_date_start +1), # calculate difference between unav start and end dates
           
           time_to_first_treat_app = case_when(
             !is.na(act_code_sent_date) ~ as.integer(act_code_sent_date - clock_start),
             TRUE ~ as.integer(first_treat_app - clock_start)), # use act code sent date if its there, or first treat app if it isnt
           
           unav_period_opti = case_when(
             
             dataset_type == "PT" &
               !is.na(unav_date_start) &
               !is.na(unav_date_end) &
               unav_date_start >= clock_start & 
               unav_date_start <= guarantee_date &
               unav_date_start < first_treat_app & # this is for PT unavailability before 18 weeks before 1st apr
               unav_date_start < "2024-04-01" ~ unav_period,
             
             dataset_type == "PT" &
               !is.na(unav_date_start) &
               !is.na(unav_date_end) &
               unav_date_start >= clock_start &
               unav_date_start < first_treat_app & # this is for PT unavailability after 1st apr
               unav_date_start >= "2024-04-01" ~ unav_period,
             
             dataset_type == "CAMHS" &
               !is.na(unav_date_start) &
               !is.na(unav_date_end) &
               unav_date_start >= clock_start &
               unav_date_start <= guarantee_date & # this is for CAMHS unavailability
               unav_date_start < first_treat_app ~ unav_period,
             
             TRUE ~ NA_real_
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
    
    slice(1) # return one row per pathway
  
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


  # flag app date without end date
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

# 
# test <- df_rtt_complete |> 
#   filter(!is.na(rtt_unadj) & is.na(rtt_adj)) |> 
#   left_join(df, by = c("ucpn", "patient_id", "hb_name", "dataset_type"))
