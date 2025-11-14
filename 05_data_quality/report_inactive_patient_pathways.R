##############################################################.
### Identifies patients in CAPTND inactive for 6/12 months ###
##############################################################.

# Author: Luke Taylor
# Date: 2025-11-06

# The script identifies patient pathways in CAPTND that have been inactive for 6 or 12
# months.

cols_vec <- c('ref_rec_date_opti', 'app_date', 'act_code_sent_date')  
#df_opti <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
#inactive_period = 12

inactive_pathways <- function(inactive_period){
  
wl_extract <- read_parquet(paste0(stats_checked_dir, "/wl_extract_", month_end, ".parquet")) |>
  select(dataset_type, hb_name, ucpn, patient_id) |>
  mutate(flag = "on WL")

df_inactive_pathways <- df_opti |>
  mutate(inactive_start = as.Date(month_start - months(inactive_period))) |>
  filter(is_case_closed == FALSE &
           #has_any_app_date == TRUE &
           is.na(ref_rej_date)) |> # have no case closed date and their referral was not rejected
  select(!!!syms(data_keys), all_of(cols_vec), inactive_start) |>
  arrange(ucpn, desc(app_date)) |>
  group_by(!!!syms(data_keys)) |>
  fill(act_code_sent_date, .direction = "updown") |> #fill rows so act_code_sent_date on all patient rows
  mutate(count = n()) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(last_act = pmax(ref_rec_date_opti, app_date, act_code_sent_date, na.rm = TRUE), #find most recent from these dates
         active = case_when(last_act >= inactive_start ~ 1, #flag if active within last 6 months
                            TRUE ~ 0),
         inactivity_length = interval(last_act, inactive_start) %/% months(1)) |>
  filter(active == 0) |>
  mutate(inactive_period = case_when(inactivity_length >= 0 & inactivity_length <= 12 ~ "12-24 months",
                                     inactivity_length > 12 & inactivity_length <= 24 ~ "25-36 months",
                                     inactivity_length > 24 & inactivity_length <= 36 ~ "37-48 months",
                                     inactivity_length > 36  ~ "Over 48 months"),
         has_appt_records = case_when(count == 1 ~ "Referral only",
                                      TRUE ~ NA)) |>
  arrange(inactive_period) |>
  select(dataset_type, hb_name, ucpn, patient_id, ref_rec_date_opti, last_act, inactive_period, has_appt_records) |>
  left_join(wl_extract, by = c("dataset_type", "hb_name", "ucpn", "patient_id")) |>
  arrange(inactive_period, flag) |>
  write_parquet(paste0(stats_checked_dir, "/inactive_patients_", month_start, ".parquet"))

}

