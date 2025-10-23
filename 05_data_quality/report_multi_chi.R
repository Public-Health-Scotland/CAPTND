#####################################################.
### Identifies pathways with multiple chi numbers ###
#####################################################.

# Author: Luke Taylor
# Date: 2025-10-02

#Identifies patient pathways with multi chi numbers submitted
#Will only flag pathways with multiple chi numbers, with the chi submitted for the first 
#time in the latest months submissions


multi_chi_pathways <- function(){
  
  multi_chi_per_pathway <- df |>
    filter(!is.na(chi)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
    mutate(latest_mth = case_when(any(!!sym(header_date_o) == month_start) ~ 1,
                                  TRUE ~ NA)) |>
    filter(latest_mth == 1) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o)) |>
    distinct() |>
    arrange(!!sym(ucpn_o)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
    mutate(count = n()) |>
    filter(count >= 2) 
  
  first_sub_chi <- df |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o), !!sym(header_date_o)) |>
    left_join(multi_chi_per_pathway, by = c('dataset_type', 'hb_name', 'ucpn', 'chi')) |>
    filter(!is.na(count),
           !is.na(ucpn)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o)) |>
    arrange(ucpn, header_date) |>
    slice_head(n = 1) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
    mutate(latest_mth = case_when(any(!!sym(header_date_o) == month_start) ~ 1,
                                  TRUE ~ NA)) |>
    filter(latest_mth == 1) |>
    select(-latest_mth) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o)) |>
    write_parquet(paste0(stats_checked_dir, "/multi_chi_pathways_", month_start, ".parquet"))
  
}

