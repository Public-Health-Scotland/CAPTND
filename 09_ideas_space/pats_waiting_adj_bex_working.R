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

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

most_recent_month_in_data <- get_lastest_month_end(df)


date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
               "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
               "unav_date_start", "unav_date_end", "header_date", "sub_month_end")

df_rtt <- df |> 
  mutate(sub_month_end = ceiling_date(header_date, unit = "month") - days(1)) |> 
  group_by(!!!syms(data_keys)) |> # for each pathway...
  mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y"))) |>
  arrange(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, app_date_o))) |>    # can we filter for app records only at this stage?
  
  # calculate basic unadjusted RTT
  mutate(pat_wait_unadj = as.integer(case_when(
    first_treat_app >= sub_month_end ~ sub_month_end - ref_rec_date_opti, # if treatment starts AFTER the end of a given sub month, unadj wait time. 
    TRUE ~ NA_integer_)))  # was ifelse for activation code or first treat app
  
  # select relevant columns
df_rtt2 <- df_rtt |> 
  select(!!!syms(c(patient_id_o, dataset_type_o, hb_name_o, ucpn_o, ref_rec_date_o, 
                   app_date_o, app_purpose_o, att_status_o, first_treat_app_o,  
                   unav_date_start_o, unav_date_end_o, unav_days_no_o, 
                   act_code_sent_date_o)), sub_month_end, pat_wait_unadj)

message('DF ready, calculating clock reset\n')



