#################################
### Check cancellation dates  ###
#################################

# month_start <- as.Date('2025-06-01')
# month_end <- as.Date('2025-06-30')
#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#Check 1 - cancelled appointment with no cancellation date
missing_cancel_dates <- function(){
  
  missing_cancel_date_df <- df |>
    filter(!!sym(att_status_o) == '03',
           is.na(!!sym(cancellation_date_o)),
           !!sym(header_date_o) == month_start) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o),
           !!sym(app_date_o), !!sym(att_status_o), !!sym(cancellation_date_o)) |>
    mutate(hb_name = case_when(hb_name == 'NHS Lanarkshire' & nchar(ucpn) == 9 ~ 'NHS Greater Glasgow and Clyde',
                               TRUE ~ hb_name)) |>
    write_parquet(paste0(stats_checked_dir, "/no_cancel_date_", month_start, ".parquet"))
  
}

#Check 2 - appointments with cancellation date, but attendance status not recorded as cancelled
cancel_date_error <- function(){
  
  cancel_date_error_df <- df |>
    filter(!(att_status %in% c('02', '03')),
           !is.na(cancellation_date),
           header_date == month_start) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o),
           !!sym(app_date_o), !!sym(att_status_o), !!sym(cancellation_date_o)) |>
    mutate(hb_name = case_when(hb_name == 'NHS Lanarkshire' & nchar(ucpn) == 9 ~ 'NHS Greater Glasgow and Clyde',
                               TRUE ~ hb_name)) |>
    write_parquet(paste0(stats_checked_dir, "/app_purp_not_can", month_start, ".parquet"))
  
}

