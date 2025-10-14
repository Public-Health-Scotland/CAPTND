#######################################################################.
### Identifies pathways with multiple ucpns submitted in same month ###
#######################################################################.

# Author: Luke Taylor
# Date: 2025-10-03

#UCPNs will change for the same CHI over time if the patient is re-referred to the service.
#However, the following script identifies pathways that have had records with multiple 
#ucpns submitted within the same month.

multi_ucpn_sub <- function(){
  
  borders_df <- df_opti |>
    mutate(flag = case_when(!!sym(hb_name_o) == 'NHS Borders' & 
                              !!sym(ref_source_o) == 6 &
                              !!sym(dataset_type_o) == 'CAMHS' ~ 1,
                            TRUE ~ 0)) |>
    filter(flag == 1) |>
    select(dataset_type, hb_name, ucpn, chi, flag) |>
    distinct()
  
  multi_ucpn_sub_df <- df |>
    mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
    left_join(borders_df, by = c("dataset_type", "hb_name", "ucpn", "chi")) |>
    filter(header_date == month_start,
           !is.na(!!sym(chi_o)),
           is.na(flag),
           !is.na(app_date)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(chi_o), !!sym(ucpn_o)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(chi_o)) |>
    mutate(count = n()) |>
    filter(count >= 2) |>
    arrange(dataset_type, hb_name, chi) |>
    mutate(header_date = as.Date(month_start)) |>
    write.xlsx(paste0(stats_checked_dir, "/multi_ucpns_sub_mth_", month_start, ".xlsx"))
    
}


