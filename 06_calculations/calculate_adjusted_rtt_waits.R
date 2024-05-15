
##############################################################.
### Calculate adjusted referral to treatment waiting times ###
##############################################################.

# Authors: Bex Madden and Charlie Smith
# Date: 2024-05-13


calculate_adjusted_rtt_waits <- function(df){
  
  # vector of date columns for easy reference
  date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
                 "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
                 "unav_date_start", "unav_date_end", "header_date")
 
  df_rtt <- df |>
    group_by(!!!syms(data_keys)) |> # for each pathway...
    mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y"))) |>
    arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, app_date_o))) |>
    
    # calculate basic unadjusted rtt wait
    mutate(rtt_unadj = as.integer(case_when(
      has_act_code_sent_date == "TRUE" ~ act_code_sent_date - ref_rec_date_opti,
      TRUE ~ first_treat_app - ref_rec_date_opti))) |> 
    
    # slect relevant variables
    select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_o, 
                     app_date_o, app_purpose_o, att_status_o, first_treat_app_o,
                     unav_date_start_o, unav_date_end_o, unav_days_no_o)), rtt_unadj) |>

    # DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
    mutate(
      dna_date = if_else(app_purpose %in% c(2, 3) &
                           att_status %in% c(3, 5, 8) &
                           app_date < first_treat_app,
                         app_date, NA_Date_), # makes a column with dates for any D/CNA/W
      
      dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
      
      dna_lag = case_when(
        is.na(dna_date) &
          !is.na(dna_lag) ~ NA_Date_,
        TRUE ~ dna_lag), # removes hanging lag date
      
      dna_lag = case_when(
        !is.na(dna_date) &
          is.na(dna_lag) ~ ref_rec_date,
        TRUE ~ dna_lag), # adds ref date as 'first' lag date
      
      dna_interval = dna_date - dna_lag, # calculates difference between one dna date and the previous dna date
      
      dna_date = if_else(dna_interval > 126, NA_Date_, dna_date), # removes 'dna_date' if the lag is more than 18weeks from the last (past guarantee)
      
      clock_start = case_when(
        any(!is.na(dna_date)) ~ max(dna_date, na.rm = TRUE),
        TRUE ~ ref_rec_date),   # make clock_start date be the max dna date, or otherwise be referral date
      
      guarantee_date = clock_start + 126) |>  # make column with the updated guarantee date
    
    
    # unavailability
    mutate(unav_date_start = case_when(clock_start > unav_date_start &
                                         clock_start < unav_date_end ~ clock_start,
                                       TRUE ~ unav_date_start),
           
           unav_period = as.integer(unav_date_end - unav_date_start), # calculate difference between unav start and end dates
           
           time_to_first_treat_app = as.integer(first_treat_app - clock_start),
           
           unav_period_opti = case_when(
             
             !is.na(unav_days_no) & xor(is.na(unav_date_start), is.na(unav_date_end)) ~ unav_days_no, # has unav_days_no but no unav start or stop date
             
             dataset_type == "PT" &
               !is.na(unav_date_start) &
               unav_date_start >= clock_start &
               unav_date_start <= guarantee_date &
               unav_date_start < first_treat_app & # this is for PT unavailability before 18 weeks before 1st apr
               unav_date_start < "2024-04-01" ~ unav_period,
             
             dataset_type == "PT" &
               !is.na(unav_date_start) &
               unav_date_start >= clock_start &
               unav_date_start < first_treat_app & # this is for PT unavailability after 1st apr
               unav_date_start >= "2024-04-01" ~ unav_period,
             
             dataset_type == "CAMHS" &
               !is.na(unav_date_start) &
               unav_date_start >= clock_start &
               unav_date_start <= guarantee_date & # this is for CAMHS unavailability
               unav_date_start < first_treat_app ~ unav_period,
             
             TRUE ~ NA_real_)) |>
    
    select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, ref_rec_date_o)), # select relevant variables
                   clock_start, unav_period_opti, time_to_first_treat_app, rtt_unadj) |> 

    distinct() |> # need to have unique so we don't artificially sum same period up
    
    mutate(unav_opti_total = sum(unav_period_opti, na.rm = TRUE), 
           
           rtt_adj = time_to_first_treat_app - unav_opti_total) |> 
    
    slice(1) # return one row per pathway
  
  return(df_rtt)
  
}



