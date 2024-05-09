camhs <- read.csv("../../../data/camhs_adjRTT_test.csv")
pt <- read.csv("../../../data/pt_adjRTT_test.csv")

both_ds <- rbind(camhs, pt)

date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
               "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
               "unav_date_start", "unav_date_end", "header_date")

df_rtt <- both_ds |>
  group_by(!!!syms(data_keys)) |>
  mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y"))) |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  
  mutate(rtt_unadj = case_when(
    has_act_code_sent_date == "TRUE" ~ act_code_sent_date - ref_rec_date_opti,
      TRUE ~ first_treat_app - ref_rec_date_opti),
         
         guarantee_date = ref_rec_date + 126) |> # make column with original guarantee date
  
 filter(ucpn == "444111111PT") |>
 select(patient_id, dataset_type, hb_name, ucpn, ref_rec_date, app_date, 
        app_purpose, att_status, first_treat_app, guarantee_date, 
        unav_date_start, unav_date_end, unav_days_no, rtt_unadj) |>

# DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
         
  mutate(#clock_start = case_when(app_purpose %in% c(2, 3) &
         #                           att_status %in% c(3, 5, 8) &
         #                           app_date < guarantee_date ~ app_date, # DNA/CNA/CNW before 18 weeks
         #                         
         #                         TRUE ~ ref_rec_date), # if no special conditions met, use referral date
         # 
         # clock_start = last(clock_start, order_by = clock_start),
         # 
         # max_dna = if_else(app_date < guarantee_date &
         #                     app_purpose %in% c(2, 3) &
         #                     att_status %in% c(3, 5, 8) &
         #                     app_date < first_treat_app,
         #                     app_date, NA_Date_),
         #                   .after = app_date,
         # max_dna = max(max_dna, na.rm = TRUE),
         
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
            TRUE ~ ref_rec_date)) |> # make clock_start date be the max dna date, or otherwise be referral date
         

  # unavailability
         
  mutate(unav_period = as.integer(unav_date_end - unav_date_start), # calculate difference between unav start and end dates
    
    # unav_period_opti = case_when( # use given unavailability days if present, or calculated if not, or NA if no unav
    # !is.na(unav_days_no) ~ unav_days_no,
    # !is.na(unav_period) &
    #   is.na(unav_days_no) ~ unav_period,
    #     TRUE ~ NA_real_), # needs to be for valid unav periods only to allow summing
    
    time_to_first_treat_app = as.integer(first_treat_app - clock_start),
    
    unav_period_opti = case_when(
      dataset_type == "PT" &
          !is.na(unav_date_start) &
          unav_date_start <= guarantee_date &
          unav_date_start < first_treat_app & # this is for PT unavailability before 18 weeks before 1st apr
          unav_date_start < "01/04/2024" ~ unav_period,
      
      dataset_type == "PT" &
        !is.na(unav_date_start) &
        unav_date_start < first_treat_app & # this is for PT unavailability after 1st apr
        unav_date_start >= "01/04/2024" ~ unav_period,
      
      dataset_type == "CAMHS" &
        !is.na(unav_date_start) &
        unav_date_start <= guarantee_date & # this is for CAMHS unavailability
        unav_date_start < first_treat_app ~ unav_period,
    
    
    # rtt_adj = case_when(
    # dataset_type == "PT" &
    #   !is.na(unav_date_start) &
    #   unav_date_start <= guarantee_date &
    #   unav_date_start < first_treat_app & # this is for PT unavailability before 18 weeks before 1st apr
    #   unav_date_start < "01/04/2024" ~ time_to_first_treat_app - unav_period_opti,
    #                          
    # dataset_type == "PT" &
    #   !is.na(unav_date_start) &
    #   unav_date_start < first_treat_app & # this is for PT unavailability after 1st apr
    #   unav_date_start >= "01/04/2024" ~ time_to_first_treat_app - unav_period_opti,
    #                          
    # dataset_type == "CAMHS" &
    #   !is.na(unav_date_start) &
    #   unav_date_start <= guarantee_date & # this is for CAMHS unavailability
    #   unav_date_start < first_treat_app ~ time_to_first_treat_app - unav_period_opti,
    # 
    # needs conditions for when only unav_days_no is populated without dates
                             
    # needs conditions for when only unav_days_no is populated without dates?
    
    TRUE ~ NA_real_),
    
    unav_opti_total = sum(unav_period_opti, na.rm = TRUE), # need to have unique so we don't aritifically sum same period up
    
    rtt_adj = time_to_first_treat_app - unav_opti_total) 
         


