####################################################.
### Identify invalid acceptance/rejection status ###
####################################################.

#This script identifies referral records that were rejected and are missing a rejection date or
# were accepted and submitted with a rejection date

#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

invalid_accept_status <- function(){
  
  rej_with_no_date_df <- df |>
    filter(!is.na(ref_acc)) |>
    mutate(valid_accept = case_when(ref_acc == '02' & is.na(ref_rej_date) ~ 'invalid',
                                    TRUE ~ 'valid')) |>
    filter(valid_accept == 'invalid',
           header_date == month_start) |>
    select(header_date, dataset_type, hb_name, ucpn, chi, ref_date, ref_rec_date, ref_acc, ref_rej_date) |>
    arrange(dataset_type, hb_name) |>
    write.xlsx(paste0(stats_checked_dir, "/rej_with_no_date_", month_start, ".xlsx"))
  
  accept_with_rej_date_df <- df |>
    filter(!is.na(ref_acc)) |>
    mutate(valid_accept = case_when(ref_acc == '01' & !is.na(ref_rej_date) ~ 'invalid',
                                    TRUE ~ 'valid')) |>
    filter(valid_accept == 'invalid',
           header_date == month_start) |>
    select(header_date, dataset_type, hb_name, ucpn, chi, ref_date, ref_rec_date, ref_acc, ref_rej_date) |>
    arrange(dataset_type, hb_name) |>
    write.xlsx(paste0(stats_checked_dir, "/accept_with_rej_date_", month_start, ".xlsx"))
  
}  


