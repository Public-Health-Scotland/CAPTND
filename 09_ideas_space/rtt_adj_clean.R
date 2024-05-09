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
  
  mutate(rtt_unadj = case_when(has_act_code_sent_date == "TRUE" ~ act_code_sent_date - ref_rec_date_opti,
                               TRUE ~ first_treat_app - ref_rec_date_opti),
         guarantee_date = ref_rec_date + 126) |>
  
 # filter(ucpn == "222111111PT") |>
  
  select(patient_id, dataset_type, hb_name, ucpn, ref_rec_date, app_date, app_purpose, att_status, first_treat_app, guarantee_date) |>

# DNA CNA         
         
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
                           app_date, NA_Date_),
          
         dna_lag = lag(dna_date, n = 1),
         
         dna_lag = case_when(is.na(dna_date) &
                               !is.na(dna_lag) ~ NA_Date_,
                             TRUE ~ dna_lag),
         
         dna_lag = case_when(!is.na(dna_date) &
                               is.na(dna_lag) ~ ref_rec_date,
                             TRUE ~ dna_lag),
         
         dna_interval = dna_date - dna_lag,
          
         dna_date = if_else(dna_interval > 126, NA_Date_, dna_date),
         
         clock_start = case_when(any(!is.na(dna_date)) ~ max(dna_date, na.rm = TRUE),
                                 TRUE ~ ref_rec_date)) 
         

  
  
  
        
  
         





# unavailability
         
  mutate(rtt_adj = case_when(dataset_type == "PT" &
                               !is.na(unav_date_start) &
                               unav_date_start < (clock_start + 126) &
                               unav_date_start < first_treat_app & # this is for PT unavailability before 18 weeks before 1st apr
                               unav_date_start < "01/04/2024" ~ ((first_treat_app - clock_start) - (unav_date_end - unav_date_start)),
                             
                             dataset_type == "PT" &
                               !is.na(unav_date_start) &
                               unav_date_start < first_treat_app & # this is for PT unavailability after 1st apr
                               unav_date_start > "01/04/2024" ~ ((first_treat_app - clock_start) - (unav_date_end - unav_date_start)),
                             
                             dataset_type == "CAMHS" &
                               !is.na(unav_date_start) &
                               unav_date_start < (clock_start + 126) & # this is for CAMHS unavailability
                               unav_date_start < first_treat_app &
                               unav_date_start < clock_start ~ ((first_treat_app - clock_start) - (unav_date_end - unav_date_start)),
                             
                             
                             TRUE ~ (first_treat_app - clock_start))) # does not work
         


