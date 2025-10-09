#######################################################################.
### Identifies pathways with multiple ucpns submitted in same month ###
#######################################################################.

# Author: Luke Taylor
# Date: 2025-10-03

#UCPNs will change for the same CHI over time if the patient is re-referred to the service.
#However, the following script identifies pathways that have had records with multiple 
#ucpns submitted within the same month.

multi_ucpn_sub <- function(){
  
  multi_ucpn_sub_df <- df |>
    mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
    filter(header_date == month_start,
           !is.na(!!sym(chi_o))) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(chi_o), !!sym(ucpn_o)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(chi_o)) |>
    mutate(count = n()) |>
    filter(count >= 2) |>
    arrange(dataset_type, hb_name, chi) |>
    mutate(header_date = as.Date(month_start)) |>
    write.xlsx(paste0(stats_checked_dir, "/multi_ucpns_sub_mth_", month_start, ".xlsx"))
    
}


