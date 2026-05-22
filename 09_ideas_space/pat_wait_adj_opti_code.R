#############################################################.
### Calculate adjusted monthly waits for patients waiting ###
#############################################################.

# Author: Luke Taylor
# Date: 2025-05-12
# Updated: 2026-05-22

# 1 Load packages and source functions--------------------------------------------------------
library(padr)
library(tictoc)

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')
source('04_check_modify/unav_period_function.R')
source('04_check_modify/clean_df_for_wl.R')

# 2 Establish time frames--------------------------------------------------------
most_recent_month_in_data <- as.Date('2026-03-01')
sub_month_start <- ymd(most_recent_month_in_data) - months(14)

pub_start_mth <- ymd(most_recent_month_in_data) - months(14)

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  clean_df_for_wl() 

# 3 Calculate adjusted patients waiting--------------------------------------------------
summarise_adj_patients_waiting <- function(){
  
  tic()
  
  dir.create(pat_waits_dir)
  
df_waiting <- df |>
  mutate(activity_date = coalesce(app_date, unav_date_start, ref_rec_date_opti),
         sub_month_end = ceiling_date(activity_date, unit = "month") - days(1),
         sub_month_start = floor_date(activity_date, unit = "month")) |>
  group_by(!!!syms(data_keys)) |> 
  arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o)), activity_date) |>
  fill(act_code_sent_date, .direction = "downup") |>
  mutate(wait_end_date = if_else(!is.na(act_code_sent_date) 
                                 & (is.na(first_treat_app) | act_code_sent_date < first_treat_app),
                                 act_code_sent_date, first_treat_app),
         wait_end_date = coalesce(wait_end_date, case_closed_opti, Sys.Date()),
         has_any_unav = any(!is.na(unav_date_start))) |>
  filter(app_date <= wait_end_date | is.na(app_date) | has_any_unav == TRUE, # filter out app dates after treatment start, only if pathway has no unav (chance that unav is on app record after treat_start)
         ref_acc_opti != "2" & is.na(ref_rej_date)) |> # remove rejected referrals
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_opti_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, cancellation_date_o, 
                   act_code_sent_date_o)), wait_end_date, sub_month_end, sub_month_start, has_any_unav) |>
  arrange(sub_month_end) |>
  filter(wait_end_date >= pub_start_mth) |>
  save_as_parquet(paste0(pat_waits_dir, "df_waiting"))

message('DF ready, calculating clock reset\n')

df_dna <- df_waiting |>
  filter(wait_end_date >= pub_start_mth) |> #need to check whether this filter is problematic
  pad(by = "sub_month_start", interval = "month") |> #alternative is tidyr::complete but this is sig slower
  mutate(sub_month_end = ceiling_date(sub_month_start, unit = "month") - days(1)) |> 
  fill(ref_rec_date_opti, .direction = "down") |> 
  mutate(dna_date = case_when(att_status == 3 ~ coalesce(cancellation_date, app_date),
                              att_status %in% c(5, 8) ~ app_date,
                              TRUE ~ NA_Date_)) |> #cancellation date added 
  filter(any(!is.na(dna_date))) |> 
  fill(dna_date, .direction = "down") |> 
  fill(c("unav_date_start", "unav_date_end"), .direction = "downup") |> # Get any unavailability filled in every row of pathway
  mutate(dna_date = coalesce(dna_date, ref_rec_date_opti)) |>  # get latest dna date in each sub month row or ref date before any dna occurs
  distinct(!!!syms(data_keys), dna_date, sub_month_end, unav_date_start, .keep_all = TRUE) |>  # removes gaps between dnas so lag doesn't get interrupted
  
  mutate(dna_lag = lag(dna_date, n = 1), # makes a lagging column of D/CNA/W dates
         dna_lag = if_else(is.na(dna_date), 
                           NA_Date_, dna_lag), # removes hanging lag date
         dna_lag = if_else(dna_date == first(dna_date), 
                           ref_rec_date_opti, dna_lag), # adds ref date as 'first' lag date 
         dna_interval = as.integer(dna_date - dna_lag)) |>    # calculates difference between one dna date and the previous dna date # QUITE A FEW WEIRD NEGATIVE BECAUSE REF REC DATE AFTER APP DATE
  
  # Get multiple unavailability in every row 
  mutate(unav_start_lag = lag(unav_date_start, n=1),
         unav_end_lag = lag(unav_date_end, n=1),
         unav_start_lag_2 = lag(unav_start_lag, n=1),
         unav_end_lag_2 = lag(unav_end_lag, n=1), # 2x lags allows for up to 3 unavailabilty periods - enough for data as of 12/2024
         #Need to do a fill??
         unav_start_lag_2 = if_else(unav_start_lag_2 == unav_date_start | unav_start_lag_2 == unav_start_lag,
                                    NA_Date_, unav_start_lag_2),
         unav_end_lag_2 = if_else(unav_end_lag_2 == unav_date_end | unav_end_lag_2 == unav_end_lag,
                                  NA_Date_, unav_end_lag_2),
         unav_start_lag = if_else(unav_start_lag == unav_date_start | unav_start_lag == unav_start_lag_2, 
                                  NA_Date_, unav_start_lag),
         unav_end_lag = if_else(unav_end_lag == unav_date_end | unav_end_lag == unav_end_lag_2,
                                NA_Date_, unav_end_lag)) |>
  save_as_parquet(paste0(pat_waits_dir, "df_dna"))


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
  
  # calculate the unavailability period
  
  mutate(dna_interval_opti = as.integer(dna_interval - unav_period_dna), # calculate the dna interval with any valid unavailability subtracted
         dna_interval_opti = coalesce(dna_interval_opti, dna_interval)) |> 
  
  filter(cumall(!dna_interval_opti >= 126)) |>  # filters for records UP TO any instance within the pathway where the interval exceeds 126 days
  
  mutate(sub_month_end_revlag = lead(sub_month_end, n = 1)) |>  # lead is a backwards lag
  
  filter(is.na(sub_month_end_revlag) | sub_month_end != sub_month_end_revlag) |> # removes any earlier dnas date rows from within the same month so only latest is counted
  select(-sub_month_end_revlag) |> 
  rename(clock_start = dna_date) |> 
  
  select(all_of(data_keys), app_date, clock_start, sub_month_end) |> # selects relevant columns # do we want app date in here?? or just 1 row per pathway
  save_as_parquet(paste0(pat_waits_dir, "df_reset"))

message('Clock reset completed, calculating pauses\n')

# unavailability logic - pausing the clock for unavailability before 18 weeks (or after for PT past 01/04/2024)

# unavailability periods cleaned and returned in df
unav_df <- unav_periods()

df_waiting_unav <- df_waiting |>
  select(-app_date, -app_purpose, -att_status) |>
  distinct() |>
  #set unav in this df to NA
  mutate(unav_date_start = NA_Date_,
         unav_date_end = NA_Date_,
         unav_days_no = as.integer(unav_date_start - unav_date_end)) |>
  #bind in updated unav df
  rbind(unav_df) |>
  
  arrange(!!!syms(data_keys), sub_month_start) |>
  
  fill("wait_end_date", .direction = "downup") |>
  filter(!is.na(wait_end_date)) |>
  
  tidyr::complete(sub_month_start = seq(min(sub_month_start), # pad df to include every month between start and end of pathway
                                        max(wait_end_date),
                                        by = "month")) |>

  mutate(sub_month_end = ceiling_date(sub_month_start, unit = "month") - days(1)) |> 
  
  fill(c("ref_rec_date_opti", "first_treat_app", "wait_end_date", "act_code_sent_date"), .direction = "downup") |> 
  
  left_join(df_reset, by = c("dataset_type", "hb_name", "patient_id", "ucpn", "sub_month_end")) |> # appends new clock start date to complete data 
  
  arrange(!!!syms(data_keys), sub_month_start, clock_start) |>
  
  fill(c("clock_start"), .direction = "down") |>
  fill(c("unav_date_start", "unav_date_end"), .direction = 'up') |>
  
  mutate(clock_start = coalesce(clock_start, ref_rec_date_opti),
         guarantee_date = clock_start + 126) |>
  
  group_by(!!!syms(data_keys), unav_period_no) |>
  
  mutate(has_unav_bef_guar = fcase(any(unav_date_start <= guarantee_date), TRUE,
                                   default = FALSE)) |>
  
  group_by(!!!syms(data_keys)) |>
  
  mutate(clock_start = coalesce(clock_start, ref_rec_date_opti),
         
         guarantee_date = clock_start + 126,
         
         unav_date_start = if_else(clock_start > unav_date_start & clock_start < unav_date_end,
                                   clock_start, unav_date_start), # if the clock start date is in the middle of an unavailability period, use it as the start of the unavailability period
         
         unav_date_start = if_else(dataset_type == "CAMHS" & unav_date_start > guarantee_date & !has_unav_bef_guar,
                                   NA_Date_, unav_date_start),
         
         unav_date_end = if_else(dataset_type == "CAMHS" & unav_date_end > guarantee_date & !has_unav_bef_guar,
                                   NA_Date_, unav_date_end),
         
         unav_days_no = if_else(is.na(unav_date_end) & is.na(unav_date_start),
                                NA_integer_,
                                unav_days_no)) |>
  
  fill(c("unav_date_start", "unav_date_end", "unav_days_no"), .direction="downup") |>
  
  select(-unav_period_no, -has_any_unav, -has_unav_bef_guar, -app_date, -cancellation_date) |>
  
  mutate(unav_days_no = as.integer(unav_date_end - unav_date_start) + 1) |>
  
  distinct() |>
  
  mutate(unav_date_start = if_else(unav_date_start >= sub_month_start & unav_date_start <= sub_month_end,
                                   unav_date_start,
                                   NA_Date_),
         unav_date_end = if_else(unav_date_end <= sub_month_end & unav_date_end >= sub_month_start,
                                   unav_date_end,
                                   NA_Date_),
         unav_date_start = if_else(is.na(unav_date_start) & !is.na(unav_date_end),
                                   sub_month_start,
                                   unav_date_start),
         unav_date_end = if_else(is.na(unav_date_end) & !is.na(unav_date_start),
                                 sub_month_end,
                                 unav_date_end),
         unav_days_no = if_else(is.na(unav_date_end) & is.na(unav_date_start),
                                NA_integer_,
                                unav_days_no)) |>
  save_as_parquet(paste0(pat_waits_dir, "df_waiting_unav"))


message('Clock pauses completed, calculating adjusted waits\n')


start_vec <- c("unav_date_start")
end_vec <- c("unav_date_end")

df_waiting_complete <- df_waiting_unav |>
  mutate(across(all_of(start_vec), ~fcase(. >= clock_start & . <= sub_month_end, ., # if start date is within the  period, keep it
                                          . <= clock_start, clock_start, # if the unavailability starts before the dna lag (start of period), use the lag date as unav start
                                          default = NA_Date_)),
         across(all_of(end_vec), ~fcase(. >= clock_start & . <= sub_month_end, ., # if the unavailability end date is within the dna period, keep it
                                        . >= sub_month_end, sub_month_end, # if the unavailability end date is after the dna date (end of period), use the dna date as the terminus
                                        default = NA_Date_)),
         
         unav_date_start = if_else(!is.na(unav_date_start) & !is.na(unav_date_end),
                                  unav_date_start,
                                  as.Date(NA)),
         
         unav_date_end = if_else(!is.na(unav_date_end) & !is.na(unav_date_start),
                                unav_date_end,
                                as.Date(NA)),
         
         unav_days_no = as.integer(unav_date_end - unav_date_start) + 1) |> 
  
  #add up unavailability periods applicable to each dna period
  mutate(valid_unav = if_else(!is.na(unav_date_start) & !is.na(unav_date_end),
                              unav_days_no,
                              NA_real_)) |>
  
  mutate_at(c('valid_unav'), ~replace_na(.,0)) |> 
  
  group_by(!!!syms(data_keys), clock_start) |>
  
  mutate(cum_unav_days = cumsum(valid_unav)) |>
  
  mutate(monthly_wait = as.integer(sub_month_end - clock_start),
         
         monthly_wait = if_else(wait_end_date <= sub_month_end,
                                NA_integer_, # insert NA if the wait end is during the submission month
                                if_else(sub_month_end == clock_start, #for patients referred on last day of month
                                        1L,
                                        monthly_wait))) |>

  filter(!is.na(monthly_wait)) |> # if their wait is now NA (i.e. they are off-list) remove them from the data
  ## IF we ever wanted to know how much of the final month a wait had continued into, paste these cases' monthly wait into new column before making them NA (or something)
  
  # select relevant variables
  select(!!!syms(c(patient_id_o, ucpn_o, dataset_type_o, hb_name_o, ref_rec_date_opti_o, 
                   unav_date_start_o, unav_date_end_o, first_treat_app_o)), wait_end_date, 
         act_code_sent_date, sub_month_end, clock_start, cum_unav_days, monthly_wait) |> 
  
  distinct() |> # keeps unique rows so we don't artificially sum same period up
  
  group_by(!!!syms(data_keys), sub_month_end) |>
  
  slice_max(cum_unav_days, n = 1) |> #remove duplicate sub_month_ends
  
  mutate(#unav_monthly_total = sum(unav_period_opti, na.rm = TRUE), 
    
    monthly_wait_adj = as.integer(monthly_wait - cum_unav_days),
    
    monthly_wait_adj = if_else(monthly_wait_adj == -1,
                               0L, #likely just a mismatch in unav days and days waiting
                               if_else(monthly_wait_adj < -1,
                                       NA_integer_,
                                       monthly_wait_adj)))

message('Adjusted waits calculated\n')

toc()

#Run time approx 50 minutes

#create parquet output to join adjusted figures onto patients waiting dataframe
df_export <- df_waiting_complete |>
  ungroup() |>
  select(!!!syms(data_keys), sub_month_end, cum_unav_days, monthly_wait, monthly_wait_adj) |>
  distinct() |>
  save_as_parquet(paste0(pat_waits_dir, "patients_waiting_adj_", month_end))

}

