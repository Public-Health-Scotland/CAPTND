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

df_rtt <- df |> 
  mutate(sub_month_end = ceiling_date(header_date, unit = "month") - days(1)) |>
  group_by(!!!syms(data_keys)) |> # for each pathway...
  arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, app_date_o))) |> 
  
  # need to filter down df before cross-join as otherwise its too demanding 
  filter(is.na(app_date) | app_date <= first_treat_app) |> # filter out app dates after treatment starts 
  filter(ref_acc != "2") |>  # filter out rejected referrals 

  # select relevant columns
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o)), sub_month_end) 

message('DF ready, calculating clock reset\n')



# DNA/CNA/CNW logic - adjusting the clock start date to account for resets   
df_reset <- df_rtt |>
  mutate(dna_date = fcase(
    att_status %in% c(3, 5, 8), app_date, 
    default = NA_Date_)) |> # makes a column with dates for any D/CNA/W # will need to add cancellation date here
  
  filter(!is.na(dna_date)) |> # removes gaps between dnas so lag doesn't get interrupted
  
  mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
         
         dna_lag = case_when(
           is.na(dna_date) &
             !is.na(dna_lag) ~ NA_Date_,
           TRUE ~ dna_lag), # removes hanging lag date
         
         dna_lag = case_when(dna_date == first(dna_date) ~ ref_rec_date, 
                         TRUE ~ dna_lag), # adds ref date as 'first' lag date 
         
         dna_interval = dna_date - dna_lag) |>    # calculates difference between one dna date and the previous dna date 
  
  filter(!any(dna_interval < 0)) |> #remove any pathway with nonsense negative dna intervals e.g. ref date after dna app date
  
  filter(cumall(!dna_interval > 126)) |>  # filters for records UP TO any instance within the pathway where the interval exceeds 126 days, CS: what does ! do - negation? - so executes as < 126? BM: yep - cumulates until an instance >126 appears
  
  mutate(clock_start = max(dna_date, na.rm = TRUE)) |>  # make clock_start date be the max remaining dna date
  
  select(all_of(data_keys), app_date, clock_start) |> # selects relevant columns # do we want app date in here?? or just 1 row per pathway
  distinct() # removes duplicates

message('Clock reset completed, calculating pauses\n')




# unavailability logic - pausing the clock for unavailability before 18 weeks (or after for PT past 01/04/2024)

df_rtt_complete <- df_rtt |>
  
  left_join(df_reset, by = c(all_of(data_keys), "app_date")) |> # appends new clock start date to complete data 
  
  fill(c("clock_start"), .direction = "downup") |> # if clock resets patient will be removed from any month's wait list prior to reset
  
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
                                     TRUE ~ unav_date_start)) |>  # if the clock start date is in the middle of an unavailability period, use it as the start of the unavailability period
         
  arrange(sub_month_end) |> 
  fill(c("unav_date_start", "unav_date_end"), .direction="down") |> # apply unav dates to all rows submitted after unav happens, so can be subtracted month-by-month # USE DOWNUP but how to remove from dates before unav?
  mutate(unav_date_end = case_when(unav_date_start < sub_month_end &
                                     unav_date_end > sub_month_end ~ sub_month_end,
                                   TRUE ~ unav_date_end), # if unavailability straddles the end of a sub month, use the sub month end as the end date # DOESN'T ACCOUNT FOR 'SKIPPED' MONTH e.g if no sub_month_end for middle month of a 3-month unav period (e.g. ucpn == "101028457061N". OR if unav hasn't been entered in the month it starts (e.g. ucpn == "1010288567129")
         
         unav_period = fcase(unav_date_end <= sub_month_end, 
                                   as.integer(unav_date_end - unav_date_start +1), # +1 as july 10-july 10 would be 0, july 10-july 11 would be 1
                                 default = NA_integer_), # calculate difference between unav start and end dates ONLY IF THE UNAV OCCURS BEFORE THE MONTH END

# CALCULATE unavailability periods to which the waiting time standard applies
         unav_period_opti = fcase(
           
           dataset_type == "PT" &
             !is.na(unav_date_start) &
             !is.na(unav_date_end) &
             unav_date_start >= clock_start & 
             unav_date_start <= guarantee_date & # this is for PT unavailability before 18 weeks before 1st apr
             unav_date_start < "2024-04-01", unav_period,
           
           dataset_type == "PT" &
             !is.na(unav_date_start) &
             !is.na(unav_date_end) &
             unav_date_start >= clock_start & # this is for PT unavailability after 1st apr
             unav_date_start >= "2024-04-01", unav_period,
           
           dataset_type == "CAMHS" &
             !is.na(unav_date_start) &
             !is.na(unav_date_end) &
             unav_date_start >= clock_start &
             unav_date_start <= guarantee_date, unav_period, # this is for CAMHS unavailability

           default = NA_integer_),
         
         unav_period_opti = case_when(!is.na(first_treat_app) &
                                        unav_date_start > first_treat_app ~ NA_integer_, # to remove any unavailability after treatment start 
                                      
                                      TRUE ~ unav_period_opti)) |>
  
  
  # select relevant variables
  select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, ref_rec_date_o,
                   unav_date_start_o, unav_date_end_o, first_treat_app_o, act_code_sent_date_o)), 
                   clock_start, unav_period_opti, sub_month_end)  |> 
  
  distinct() # keeps unique rows so we don't artifically sum same period up
  
message('RTT adjustment completed!\n')


save_as_parquet(df_rtt_complete, "../bex_test/pats_waiting_adj_wholedata_test")
test5 <- filter(df_rtt_complete, any(!is.na(unav_date_start)))
# DOESN'T ACCOUNT FOR 'SKIPPED' MONTH e.g if no sub_month_end for middle month of a 3-month unav period (e.g. ucpn == "101028457061N". OR if unav hasn't been entered in the month it starts (e.g. ucpn == "1010288567129")
