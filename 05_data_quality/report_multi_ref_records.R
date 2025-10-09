###########################################################.
### Identifies pathways with multiple referral records ###
###########################################################.

# Author: Luke Taylor
# Date: 2025-10-02

# The script will highlight:
# Pathways with more than one than ref_rec_date
# Pathways with the same ref_rec_date, but multiple referral records
# Pathways with same referral record submitted in the same or different months

multi_ref_pathways <- function(){
  
multi_ref_per_pathway <- df |>
  filter(!is.na(!!sym(ref_rec_date_o)) & is.na(!!sym(ref_rej_date_o))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o)) |>
  mutate(latest_mth = case_when(any(!!sym(header_date_o) == month_start) ~ 1,
                                TRUE ~ NA)) |>
  filter(latest_mth == 1) |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o),
         !!sym(ref_rec_date_o), !!sym(header_date_o), !!sym(ref_source_o)) |>
  arrange(!!sym(ucpn_o), !!sym(ref_rec_date_o)) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o)) |>
  mutate(count = n()) |>
  filter(count >= 2,
         hb_name != 'NHS24') |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
  write.xlsx(paste0(stats_checked_dir, "/multi_ref_records_", month_start, ".xlsx"))

}
  