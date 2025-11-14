#############################################################################
### Appointment records submitted without appt purpose/attendance status  ###
#############################################################################

#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#Identifies appointment records submitted in the latest month that are missing
# an appt purpose or do not have an attendance status

missing_appt_purpose <- function(){
  
  missing_appt_purpose_df <- df |>
    filter(!is.na(app_date) & is.na(app_purpose) | app_purpose == 99,
           header_date == month_start) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(app_date_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o), 
           !!sym(app_date_o), !!sym(app_purpose_o), !!sym(att_status_o), !!sym(header_date_o)) |>
    write_parquet(paste0(stats_checked_dir, "/missing_appt_purpose_", month_start, ".parquet"))
  
}

missing_att_status <- function(){
  
  missing_att_status_df <- df |>
    filter(!is.na(app_date) & is.na(att_status) | att_status == 99,
           header_date == month_start) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(app_date_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o), 
           !!sym(app_date_o), !!sym(app_purpose_o), !!sym(att_status_o), !!sym(header_date_o)) |>
    write_parquet(paste0(stats_checked_dir, "/missing_att_status_", month_start, ".parquet"))
  
}