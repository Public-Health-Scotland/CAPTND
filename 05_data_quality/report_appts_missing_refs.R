####################################################################################.
### Identifies appointment records submitted for pathway without referral record ###
####################################################################################.

# Author: Luke Taylor
# Date: 2025-10-01

# The script identifies appointments that have been submitted in a given month for a pathway that
# do not have a referral record

assess_appts_missing_refs <- function(){
  
  #identify all assessment and review appts in received in latest submission month
  assess_appts_missing_refs <- df |>
    mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
    group_by(dataset_type, hb_name, ucpn, chi) |>
    mutate(has_ref_record = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                  default = FALSE)) |>
    arrange(ucpn, app_date) |>
    filter(has_ref_record == FALSE,
           !is.na(app_date),
           app_purpose %in% c('01','04'),
           header_date == month_start) |>
    select(dataset_type, hb_name, ucpn, chi, app_date, app_purpose, header_date) |>
    distinct() |>
    filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
           !is.na(chi) & chi != "0" & chi != "NULL") |>
    arrange(dataset_type, hb_name, ucpn, app_date)
  
  #isolate the data keys 
  data_keys_df <- assess_appts_missing_refs |>
    mutate(flag = 1) |>
    select(dataset_type, hb_name, ucpn, chi, flag) |>
    distinct()
  
  #identify the first contact appointment
  first_app_date_df <- df |>
    left_join(data_keys_df, by = c("dataset_type", "hb_name", "chi", "ucpn")) |>
    filter(flag == 1) |>
    group_by(dataset_type, hb_name, chi, ucpn) |>
    arrange(ucpn, app_date) |>
    slice_head(n = 1) |> ungroup() |>
    select(dataset_type, hb_name, chi, ucpn, first_con_app_date = app_date) 
  
  #attached first contact appt date to list of assessment appts from most recent month
  assess_appts_missing_refs_complete <- assess_appts_missing_refs |>
    left_join(first_app_date_df, by = c("dataset_type", "hb_name", "chi", "ucpn")) |>
    relocate(header_date, .after = last_col()) |>
    arrange(dataset_type, hb_name, ucpn, first_con_app_date) |>
    filter(first_con_app_date >= '2023-01-01',
           !(hb_name == 'NHS Lanarkshire' & nchar(ucpn) == 9)) |>
    write_parquet(paste0(stats_checked_dir, "/assess_appts_missing_ref_", month_start, ".parquet"))
  
}


treat_appts_missing_refs <- function(){
  
  treat_appts_missing_refs <- df |>
    mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
    group_by(dataset_type, hb_name, ucpn, chi) |>
    mutate(has_ref_record = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                  default = FALSE)) |>
    arrange(ucpn, app_date) |>
    filter(has_ref_record == FALSE,
           !is.na(app_date),
           app_purpose %in% c('02','03','05'),
           header_date == month_start) |>
    select(dataset_type, hb_name, ucpn, chi, app_date, app_purpose, header_date) |>
    distinct() |>
    filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
           !is.na(chi) & chi != "0" & chi != "NULL") |>
    arrange(dataset_type, hb_name, ucpn, app_date)
  
  data_keys_df <- treat_appts_missing_refs |>
    mutate(flag = 1) |>
    select(dataset_type, hb_name, ucpn, chi, flag) |>
    distinct()
  
  first_app_date_df <- df |>
    left_join(data_keys_df, by = c("dataset_type", "hb_name", "chi", "ucpn")) |>
    filter(flag == 1) |>
    group_by(dataset_type, hb_name, chi, ucpn) |>
    arrange(ucpn, app_date) |>
    slice_head(n = 1) |> ungroup() |>
    select(dataset_type, hb_name, chi, ucpn, first_con_app_date = app_date) 
  
  treat_appts_missing_refs_complete <- treat_appts_missing_refs |>
    left_join(first_app_date_df, by = c("dataset_type", "hb_name", "chi", "ucpn")) |>
    relocate(header_date, .after = last_col()) |>
    arrange(dataset_type, hb_name, ucpn, first_con_app_date) |>
    filter(first_con_app_date >= '2023-01-01',
           !(hb_name == 'NHS Lanarkshire' & nchar(ucpn) == 9)) |>
    write_parquet(paste0(stats_checked_dir, "/treat_appts_missing_ref_", month_start, ".parquet"))
  
}
