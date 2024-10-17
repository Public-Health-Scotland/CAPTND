###############################################.
### Patients Waiting - wait time adjustment ###
###############################################.

# Auhtor: Bex Madden
# Date: 3/10/2024

# Need row-wise data so start with main df
# 'stop' at end of each submission month
# avoid slices
# keep months in everything - then distinct() should be ok
# [atient will have ref date, possibly multiple app dates with assessment or dna, 
# unavailability, all before first_tret_app
# dont trim down to date range until after adjustment is done

# same principles:
# clock start date which adjusts with dnas
# unavailability for pauses
# clock stop is rolling month end NOT treatment app
# Aim to get clock_start and unavailability period (applicable monthly) out to join into summarise_patients_waiting

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
               "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
               "unav_date_start", "unav_date_end", "header_date", "sub_month_end")

df_rtt <- df |> 
  mutate(sub_month_end = ceiling_date(header_date, unit = "month") - days(1)) |> 
  group_by(!!!syms(data_keys)) |> # for each pathway...
  mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y"))) |> # CS: is this needed? I think all the dates are already formatted as yyyy-mm-dd dates
  arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, app_date_o))) |> 
  
  # need to filter down df before cross-join as otherwise its too demanding CHECK THIS IS OK
  filter(is.na(app_date) | app_date <= first_treat_app) |> # filter out app dates after treatment starts
  filter(ref_acc != "2") |>  # filter out rejected referrals # CANT filter for header date within the desired range as might be dna/unav earlier in wait that factors into adjustment
  
  # cross_join won't work need to pad the existing sub_month_end column with missing months in the range - provide clock start and dated unavailability and deal with the monthly steps in summarise_patients_waiting
    # calculate basic unadjusted RTT not necessary in the patients waiting version as already done in summarise_patients_waiting
  # select relevant columns
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o)), sub_month_end) #pat_wait_unadj

message('DF ready, calculating clock reset\n')



# DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
df_reset <- df_rtt |>
  mutate(dna_date = if_else(
    att_status %in% c(3, 5, 8),# &
      #app_date < first_treat_app, # scrapped in waiting version
    app_date, NA_Date_)) |> # makes a column with dates for any D/CNA/W # will need to add cancellation date here
  
  filter(!is.na(dna_date)) |> # removes gaps between dnas so lag doesn't get interrupted
  
  mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
         
         dna_lag = case_when(
           is.na(dna_date) &
             !is.na(dna_lag) ~ NA_Date_,
           TRUE ~ dna_lag), # removes hanging lag date
         
         dna_lag = if_else(dna_date == first(dna_date), 
                           ref_rec_date, dna_lag), # adds ref date as 'first' lag date 
         
         dna_interval = dna_date - dna_lag) |>    # calculates difference between one dna date and the previous dna date # QUITE A FEW WEIRD NEGATIVE BECAUSE REF REC DATE AFTER APP DATE
  
  filter(is.na(first_treat_app) | first_treat_app > dna_date) |>  # Only for dnas before treatment start (for records where there is a treatment start)
  
  filter(cumall(!dna_interval > 126)) |>  # filters for records UP TO any instance within the pathway where the interval exceeds 126 days, CS: what does ! do - negation? - so executes as < 126?
  
  mutate(clock_start = max(dna_date, na.rm = TRUE)) |>  # make clock_start date be the max remaining dna date
  
  select(all_of(data_keys), app_date, clock_start) |> # selects relevant columns # do we want app date in here?? or just 1 row per pathway
  distinct() # removes duplicates

message('Clock reset completed, calculating pauses\n')




# unavailability logic - pausing the clock for unavailability before 18 weeks (or after for PT past 01/04/2024)

df_rtt_complete <- df_rtt |>
  
  left_join(df_reset, by = c(all_of(data_keys), "app_date")) |> # appends new clock start date to complete data 
  
  fill(c("clock_start"), .direction = "downup") |> # OK FOR CLOCK START TO BE USED INDISCRIMINATELY AS PATIENT SHOULD BE REMOVED FROM PATS WAITING COUNT FOR ANY MONTH BEFORE RESET
  
  mutate(clock_start = case_when(is.na(clock_start) ~ ref_rec_date,
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
         
         unav_date_end = case_when(unav_date_start < sub_month_end &
                                     unav_date_end > sub_month_end ~ sub_month_end,
                                   TRUE ~ unav_date_end), # anything for na date end?
         
         unav_period = case_when(unav_date_end <= sub_month_end ~ 
                                   as.integer(unav_date_end - unav_date_start +1), #PLUS 1 AS 10TH JULY-10TH JULY WOULD BE 0 # CS: better to add +1 only when the dates match? Otherwise all other difftimes off by +1
                                 TRUE ~ NA_integer_), # calculate difference between unav start and end dates #ONLY IF THE UNAV OCCURS BEFORE THE MONTH END (anything after treatment has started will get trimmed off later)
         
         # time_to_first_treat_app = case_when( # NOT NEEDED FOR PATS WAITING - will do the actual wait time calculation in summarise patients waiting
       
# CALCULATE unavailability periods to which the waiting time standard applies
         unav_period_opti = case_when(
           
           dataset_type == "PT" &
             !is.na(unav_date_start) &
             !is.na(unav_date_end) &
             unav_date_start >= clock_start & 
             unav_date_start <= guarantee_date & # this is for PT unavailability before 18 weeks before 1st apr
            # unav_date_start < first_treat_app & 
             unav_date_start < "2024-04-01" ~ unav_period,
           
           dataset_type == "PT" &
             !is.na(unav_date_start) &
             !is.na(unav_date_end) &
             unav_date_start >= clock_start & # this is for PT unavailability after 1st apr
           #  unav_date_start < first_treat_app & 
             unav_date_start >= "2024-04-01" ~ unav_period,
           
           dataset_type == "CAMHS" &
             !is.na(unav_date_start) &
             !is.na(unav_date_end) &
             unav_date_start >= clock_start &
             unav_date_start <= guarantee_date ~ unav_period, # this is for CAMHS unavailability
             #unav_date_start < first_treat_app 
           
           TRUE ~ NA_real_
         ),
         
         unav_period_opti = case_when(!is.na(first_treat_app) &
                                        unav_date_start > first_treat_app ~ NA_integer_, # to remove any unavailability after treatment start 
                                      
                                      TRUE ~ unav_period_opti)) |>
  
  
  # select relevant variables
  select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, ref_rec_date_o,
                   unav_date_start_o, unav_date_end_o, first_treat_app_o, act_code_sent_date_o)), 
         clock_start, unav_period_opti, sub_month_end)  |> 
  
  distinct() #|> # keeps unique rows so we don't artifically sum same period up
  
  #mutate(unav_opti_total = sum(unav_period_opti, na.rm = TRUE), 
         
  #       rtt_adj = time_to_first_treat_app - unav_opti_total) |> # think we want to do this in summarise_aptients_waiting
  
  #slice(1) # return one row per pathway NO SLICE in patients waiting

message('RTT adjustment completed!\n')

