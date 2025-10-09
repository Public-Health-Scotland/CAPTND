####################################################################################.
### Identifies appointment records submitted for pathway without referral record ###
####################################################################################.

# Author: Luke Taylor
# Date: 2025-10-01

# The script identifies appointments that have been submitted in a given month for a pathway that
# does not have a referral record

appts_missing_refs <- function(){
  
  appts_missing_refs_df <- df |>
    group_by(dataset_type, hb_name, ucpn, chi) |>
    mutate(has_ref_record = fcase(any(!is.na(ref_date) | any(!is.na(ref_rec_date))), TRUE,
                                  default = FALSE)) |>
    arrange(ucpn, app_date) |>
    filter(has_ref_record == FALSE,
           !is.na(app_date)) |>
    select(dataset_type, hb_name, ucpn, chi, header_date) |>
    distinct() |>
    filter(header_date == month_start,
           !is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
           !is.na(chi) & chi != "0" & chi != "NULL") |>
    arrange(dataset_type, hb_name) |>
    write.xlsx(paste0(stats_checked_dir, "/appts_missing_ref_", month_start, ".xlsx"))
  
}
