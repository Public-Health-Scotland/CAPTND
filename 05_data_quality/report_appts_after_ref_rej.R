##################################################################################.
### Identifies appointment records submitted for pathway with rejected referral ###
##################################################################################.

# Author: Luke Taylor
# Date: 2025-10-06

# The script identifies appointments that have been submitted in a given month for a pathway
# with a referral rejection.

appts_after_rej_ref <- function(){

appts_after_rej_ref_df <- df |>
  group_by(!!sym(hb_name_o), !!sym(dataset_type_o), !!sym(ucpn_o), !!(chi_o)) |>
  mutate(has_rej_ref = fcase(any(ref_acc == '02'), TRUE,
                             default = FALSE)) |>
  fill(ref_rej_date, .direction = "downup") |>
  fill(ref_rec_date, .direction = "downup") |>
  ungroup() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL",
         has_rej_ref == TRUE & !is.na(app_date),
         header_date == month_start) |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date, ref_rej_date, app_date, header_date) |>
  distinct() |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  write_parquet(paste0(stats_checked_dir, "/appts_after_rej_ref_", month_start, ".parquet"))
  
}
