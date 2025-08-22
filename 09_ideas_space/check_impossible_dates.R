###########################################################
### Identify impossible dates + case closed date errors ###
###########################################################

# month_start <- as.Date('2025-06-01')
# month_end <- as.Date('2025-06-30')
# df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

#Check 1 - app_date before referral received date

impossible_app_dates <- function(){
df_check_app_dates <- df |>
  filter(app_date < ref_rec_date_opti,
         header_date >= month_start & header_date <= month_end) |>
  select(!!!syms(data_keys), app_date, ref_rec_date_opti, header_date) |>
  write.xlsx(paste0(stats_checked_dir, "/impossible_appts_", month_start, ".xlsx"))
}

#Check 2 - case closed dates before ref_rec_date_opti
impossible_case_closed_dates <- function(){
df_check_cc_ref <- df |>
  filter(case_closed_date < ref_rec_date_opti,
         ref_rec_date_opti >= month_start & ref_rec_date_opti <= month_end) |>
  select(!!!syms(data_keys), ref_rec_date_opti, case_closed_date, header_date) |>
  distinct() |>
  write.xlsx(paste0(stats_checked_dir, "/impossible_cc_ref_", month_start, ".xlsx"))
}

#Check 3 - case_closed_date before attended appts
invalid_case_closed_dates <- function(){
df_check_cc_app <- df |>
  filter(!is.na(app_date) & att_status == 1,
         case_closed_date < app_date,
         header_date >= month_start & header_date <= month_end) |>
  select(!!!syms(data_keys), app_date, case_closed_date, case_closed_opti, header_date) |>
  distinct() |>
  write.xlsx(paste0(stats_checked_dir, "/impossible_cc_app_", month_start, ".xlsx"))
}

#Check 4 - number of case_closed_dates
multiple_case_closed_dates <- function(){
df_check_cc_no <- df |>
  filter(is_case_closed == TRUE) |>
  select(!!!syms(data_keys), case_closed_date) |>
  distinct() |>
  group_by(!!!syms(data_keys)) |>
  mutate(cc_n = n(),
         had_cc_date_last_mth = fcase(any(case_closed_date >= month_start), TRUE,
                                      default = FALSE)) |>
  filter(cc_n > 1,
         had_cc_date_last_mth == TRUE) |>
  select(!!!syms(data_keys), case_closed_date, cc_n) |>
  write.xlsx(paste0(stats_checked_dir, "/multi_cc_dates_", month_start, ".xlsx"))
}


#Check 5 - check for duplicate UCPNs submitted under different CHI/UPIs
identify_duplicate_ucpns <- function(){
  
  df_dup_ucpn <- df |>
    arrange(ucpn, postcode) |>
    group_by(!!!syms(data_keys)) |>
    slice_head(n = 1) |>
    select(!!!syms(data_keys), ref_rec_date_opti) |>
    distinct() |>
    group_by(dataset_type, hb_name, ucpn) |>
    mutate(n_chi = n()) |>
    filter(n_chi >= 2,
           ref_rec_date_opti >= month_start) |>
    write.xlsx(paste0(stats_checked_dir, "/duplicate_ucpns_", month_start, ".xlsx"))
  
}
