##################################
### Identify impossible dates  ###
##################################

# month_start <- as.Date('2025-06-01')
# month_end <- as.Date('2025-06-30')
df_opti <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))  

#Check 1 - app_date before referral received date
#Highlights any appointment dates submitted that are before the referral received date
impossible_app_dates <- function(){
  
  month_end <- ceiling_date(month_start, 'month') - days(1)
  
  impossible_app_dates_df <- df_opti |>
    filter(!!sym(app_date_o) < !!sym(ref_rec_date_opti_o),
           !!sym(header_date_o) >= month_start & !!sym(header_date_o) <= month_end) |>
    select(!!!syms(data_keys), !!sym(app_date_o), !!sym(ref_rec_date_opti_o), !!sym(header_date_o)) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    write_parquet(paste0(stats_checked_dir, "/impossible_appts_", month_start, ".parquet"))

}

#Check 2 - case closed dates before ref_rec_date_opti
#Highlights any case closed dates submitted that are before the referral received date
impossible_case_closed_dates <- function(){
  
  month_end <- ceiling_date(month_start, 'month') - days(1)
  
  impossible_cc_dates_df <- df_opti |>
    filter(!!sym(case_closed_date_o) < !!sym(ref_rec_date_opti_o),
           !!sym(ref_rec_date_opti_o) >= month_start & !!sym(ref_rec_date_opti_o) <= month_end,
           !!sym(header_date_o) >= month_start & !!sym(header_date_o) <= month_end) |>
    select(!!!syms(data_keys), !!sym(ref_rec_date_opti_o), !!sym(case_closed_date_o), !!sym(header_date_o)) |>
    distinct() |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    write_parquet(paste0(stats_checked_dir, "/impossible_cc_ref_", month_start, ".parquet"))

}

