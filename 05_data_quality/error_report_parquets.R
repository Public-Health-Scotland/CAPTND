
### Missing attendace status ###

source("05_data_quality/plot_error_report_data_function.R")

#previous months df
missing_att_status <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/missing_att_status.parquet")

#new months data
new_missing_att_status <- read_parquet(paste0(stats_checked_dir, "/missing_att_status_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
missing_att_status <- bind_rows(missing_att_status, new_missing_att_status) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(missing_att_status, header_date, "Missing attendance status count by health board")

#write new parquet file
save_as_parquet(missing_att_status, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/missing_att_status")



### Rejected referral with no rejection date ###

#previous months df
rej_with_no_date <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/rej_with_no_date.parquet")

#new months data
new_rej_with_no_date <- read_parquet(paste0(stats_checked_dir, "/rej_with_no_date_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
rej_with_no_date <- bind_rows(rej_with_no_date, new_rej_with_no_date) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
#plot_error_report_data(rej_with_no_date, header_date, "Rejected referral with no date count by health board")

#write new parquet file
save_as_parquet(rej_with_no_date, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/rej_with_no_date")



### Accepted referral with rejection date ###

#previous months df
accept_with_rej_date <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/accept_with_rej_date.parquet")

#new months data
new_accept_with_rej_date <- read_parquet(paste0(stats_checked_dir, "/accept_with_rej_date_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
accept_with_rej_date <- bind_rows(accept_with_rej_date, new_accept_with_rej_date) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
#plot_error_report_data(accept_with_rej_date, header_date, "Accepted referral with a rejection date count by health board")

#write new parquet file
save_as_parquet(accept_with_rej_date, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/accept_with_rej_date")



### Impossible appointment dates ###

#previous months df
impossible_app_dates <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/impossible_app_dates.parquet")

#new months data
new_impossible_app_dates <- read_parquet(paste0(stats_checked_dir, "/impossible_appts_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
impossible_app_dates <- bind_rows(impossible_app_dates, new_impossible_app_dates) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(impossible_app_dates, header_date, "Impossible appointment date count by health board")

#write new parquet file
save_as_parquet(impossible_app_dates, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/accept_with_rej_date")



### Impossible case closed dates ###

#previous months df
impossible_cc_dates <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/impossible_cc_dates.parquet")

#new months data
new_impossible_cc_dates <- read_parquet(paste0(stats_checked_dir, "/impossible_cc_ref_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
impossible_cc_dates <- bind_rows(impossible_cc_dates, new_impossible_cc_dates) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(impossible_cc_dates, header_date, "Impossible case closed date count by health board")

#write new parquet file
save_as_parquet(impossible_cc_dates, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/impossible_cc_dates")



### Missing cancellation dates ###

#previous months df
missing_cancel_date <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/missing_cancel_date.parquet")

#new months data
new_missing_cancel_date <- read_parquet(paste0(stats_checked_dir, "/no_cancel_date_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
missing_cancel_date <- bind_rows(missing_cancel_date, new_missing_cancel_date) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(missing_cancel_date, header_date, "Missing cancellation date count by health board")

#write new parquet file
save_as_parquet(missing_cancel_date, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/missing_cancel_date")



### Cancellation date error ###

#previous months df
cancel_date_error <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/cancel_date_error.parquet")

#new months data
new_cancel_date_error <- read_parquet(paste0(stats_checked_dir, "/app_purp_not_can", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
cancel_date_error <- bind_rows(cancel_date_error, new_cancel_date_error) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(cancel_date_error, header_date, "Cancellation date error count by health board")

#write new parquet file
save_as_parquet(cancel_date_error, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/cancel_date_error")



### Invalid unavailability ###

#previous months df
unav_validity <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/unav_validity.parquet")

#new months data
new_unav_validity <- read_parquet(paste0(stats_checked_dir, "/invalid_unav", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
unav_validity <- bind_rows(unav_validity, new_unav_validity) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(unav_validity, header_date, "Invalid unavailability count by health board")

#write new parquet file
save_as_parquet(unav_validity, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/unav_validity")



### Missing ethnicity ###

#previous months df
missing_ethnicity <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/missing_ethnicity.parquet")

#new months data
new_missing_ethnicity <- read_parquet(paste0(stats_checked_dir, "/missing_ethnicity_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
missing_ethnicity <- bind_rows(missing_ethnicity, new_missing_ethnicity) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(missing_ethnicity, header_date, "Missing ethnicity count by health board")

#write new parquet file
save_as_parquet(missing_ethnicity, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/missing_ethnicity")



### Appts after rejected referral ###

#previous months df
appts_after_rej_ref <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/appts_after_rej_ref.parquet")

#new months data
new_appts_after_rej_ref <- read_parquet(paste0(stats_checked_dir, "/appts_after_rej_ref_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
appts_after_rej_ref <- bind_rows(appts_after_rej_ref, new_appts_after_rej_ref) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(appts_after_rej_ref, header_date, "Appts after rejected referral count by health board")

#write new parquet file
save_as_parquet(appts_after_rej_ref, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/appts_after_rej_ref")



### Multiple referral records ###

#previous months df
multi_ref_records<- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/multi_ref_records.parquet")

#new months data
new_multi_ref_records <- read_parquet(paste0(stats_checked_dir, "/multi_ref_records_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup() |>
  mutate(reporting_mth = month_start)
  
#combine data
multi_ref_records <- bind_rows(multi_ref_records, new_multi_ref_records) |>
  arrange(dataset_type, hb_name, reporting_mth) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(multi_ref_records, reporting_mth, "Multiple referral record count by health board")

#write new parquet file
save_as_parquet(multi_ref_records, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/multi_ref_records")



### Multiple CHI pathways ###

#previous months df
multi_chi_pathways <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/multi_chi_pathways.parquet")

#new months data
new_multi_chi_pathways <- read_parquet(paste0(stats_checked_dir, "/multi_chi_pathways_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup() |>
  mutate(reporting_mth = month_start)

#combine data
multi_chi_pathways <- bind_rows(multi_chi_pathways, new_multi_chi_pathways) |>
  arrange(dataset_type, hb_name, reporting_mth) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(multi_chi_pathways, reporting_mth, "Multiple CHI pathway count by health board")

#write new parquet file
save_as_parquet(multi_chi_pathways, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/multi_chi_pathways")



### Assessment appointments missing referral record ###

#previous months df
assess_appts_missing_refs <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/assess_appts_missing_refs.parquet")

#new months data
new_assess_appts_missing_refs <- read_parquet(paste0(stats_checked_dir, "/assess_appts_missing_ref_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
assess_appts_missing_refs <- bind_rows(assess_appts_missing_refs, new_assess_appts_missing_refs) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(assess_appts_missing_refs, header_date, "Assessment appts missing referral count by health board")

#write new parquet file
save_as_parquet(assess_appts_missing_refs, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/assess_appts_missing_refs")



### Treatment appointments missing referral record ###

#previous months df
treat_appts_missing_refs <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/treat_appts_missing_refs.parquet")

#new months data
new_treat_appts_missing_refs <- read_parquet(paste0(stats_checked_dir, "/treat_appts_missing_ref_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name, header_date) |>
  summarise(count = n()) |> ungroup()

#combine data
treat_appts_missing_refs <- bind_rows(treat_appts_missing_refs, new_treat_appts_missing_refs) |>
  arrange(dataset_type, hb_name, header_date) |>
  mutate(header_date = as.Date(header_date)) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(treat_appts_missing_refs, header_date, "Treatment appts missing referral count by health board")

#write new parquet file
save_as_parquet(treat_appts_missing_refs, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/treat_appts_missing_refs")



### Patient waiting list ###

#previous months df
wl_extract <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/wl_extract.parquet")

#new months data
new_wl_extract <- read_parquet(paste0(stats_checked_dir, "/wl_extract_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup() |>
  mutate(header_date = month_start)

#combine data
wl_extract <- bind_rows(wl_extract, new_wl_extract) |>
  arrange(dataset_type, hb_name, header_date) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(wl_extract, header_date, "Waiting list count by health board")

#write new parquet file
save_as_parquet(wl_extract, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/wl_extract")



### Inactive patient list ###

#previous months df
inactive_patients <- read_parquet("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/inactive_patients.parquet")

#new months data
new_inactive_patients <- read_parquet(paste0(stats_checked_dir, "/inactive_patients_", month_start, ".parquet")) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup() |>
  mutate(header_date = month_start)

#combine data
inactive_patients <- bind_rows(inactive_patients, new_inactive_patients) |>
  arrange(dataset_type, hb_name, header_date) |>
  distinct()

#create graph to visualise changes
plot_error_report_data(inactive_patients, header_date, "Inactive patient count by health board")

#write new parquet file
save_as_parquet(inactive_patients, "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/error_report_agg/inactive_patients")
