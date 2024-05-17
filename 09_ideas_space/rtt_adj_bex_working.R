
camhs <- read.csv("../../../data/camhs_adjRTT_test.csv")
pt <- read.csv("../../../data/pt_adjRTT_test.csv")

both_ds <- rbind(camhs, pt)
#both_ds <- read.csv("../../../data/adj_rtt_test_2.csv")


date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
               "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
               "unav_date_start", "unav_date_end", "header_date")

df_rtt <- both_ds |>
  group_by(!!!syms(data_keys)) |>
  mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y"))) |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  
  mutate(rtt_unadj = as.integer(case_when(
    has_act_code_sent_date == "TRUE" ~ act_code_sent_date - ref_rec_date_opti,
    TRUE ~ first_treat_app - ref_rec_date_opti))) |> 
  
  #filter(ucpn == "101024873068D") |>
  select(patient_id, dataset_type, hb_name, ucpn, ref_rec_date, app_date, 
         app_purpose, att_status, first_treat_app,  
         unav_date_start, unav_date_end, unav_days_no, rtt_unadj) 
  
  # DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
  
  df_reset <- df_rtt |>
    mutate(dna_date = if_else(app_purpose %in% c(2, 3) &
                         att_status %in% c(3, 5, 8) &
                         app_date < first_treat_app,
                       app_date, NA_Date_)) |> # makes a column with dates for any D/CNA/W
  
  filter(!is.na(dna_date)) |> # ADDED THIS - removes gaps in dnas so lag doesn't get interrupted
    
    mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
    
    dna_lag = case_when(
      is.na(dna_date) &
        !is.na(dna_lag) ~ NA_Date_,
      TRUE ~ dna_lag), # removes hanging lag date
    
    dna_lag = if_else(dna_date == first(dna_date), 
                           ref_rec_date, dna_lag), # adds ref date as 'first' lag date 
    
    dna_interval = dna_date - dna_lag, # calculates difference between one dna date and the previous dna date
  
  #slice(1:which(dna_interval > 126, arr.ind = TRUE)) |>
    
    dna_interval_2 = row_number(),
    
    dna_interval_3 = if_else(dna_interval < 126, as.integer(dna_interval), 0),
    
    #fill(dna_interval_3, .direction = "down")#,
    
    dna_interval_4 = cumsum(dna_interval > 126), # this works but why?
    
    dna_date_test = if_else(dna_interval_4 == 0, dna_date, NA_Date_), # removes 'dna_date' if the lag is more than 18weeks from the last (past guarantee)
    
    clock_start = case_when(
      any(!is.na(dna_date_test)) ~ max(dna_date_test, na.rm = TRUE),
      TRUE ~ ref_rec_date),   # make clock_start date be the max dna date, or otherwise be referral date
    
    guarantee_date = clock_start + 126) |>  # make column with the updated guarantee date
    
    select(all_of(data_keys), clock_start, guarantee_date, app_date) |>
    distinct()
  
  
  # unavailability
  
  df_rtt_test <- df_rtt |>
    
    left_join(df_reset, by = c(all_of(data_keys), "app_date")) |>
    
    fill(c("clock_start", "guarantee_date"), .direction = "downup") |>
    
  mutate(clock_start = case_when(is.na(clock_start) ~ ref_rec_date,
                                 TRUE ~ clock_start),
    
    unav_date_start = case_when(clock_start > unav_date_start &
                                      clock_start < unav_date_end ~ clock_start,
                                      TRUE ~ unav_date_start),
         
         unav_period = as.integer(unav_date_end - unav_date_start), # calculate difference between unav start and end dates
         
         time_to_first_treat_app = as.integer(first_treat_app - clock_start),
         
         unav_period_opti = case_when(
           !is.na(unav_days_no) & xor(is.na(unav_date_start), is.na(unav_date_end)) ~ unav_days_no,
           
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
           
           TRUE ~ NA_real_
         )) |>
  
  select(patient_id, ucpn, dataset_type, hb_name, clock_start, ref_rec_date, unav_period_opti, time_to_first_treat_app, rtt_unadj) |> 
  distinct() |> # need to have unique so we don't artifically sum same period up
  
  mutate(unav_opti_total = sum(unav_period_opti, na.rm = TRUE), 
         
         rtt_adj = time_to_first_treat_app - unav_opti_total) |> 
  
  slice(1) # return one row per pathway



# CHECKING

# load manually calculated figs for checking
df_check <- import('../../../data/adjRTT_test_notes.xlsx') |> 
  row_to_names(row = 1) |> 
  rename(dataset_type = `DS type`,
         ucpn = UCPN,
         unadj_wait_manual = `unadj wait`,
         adj_wait_manual = `adj wait`,
         notes = Notes) |> 
  filter(! dataset_type %in% c("DS type", NA_character_))


# join for checking
test <- df_rtt |> 
  left_join(df_check, by = c("dataset_type", "ucpn")) |> 
  mutate(adj_rtt_match = rtt_adj == adj_wait_manual) |> 
  filter(adj_rtt_match == FALSE)




