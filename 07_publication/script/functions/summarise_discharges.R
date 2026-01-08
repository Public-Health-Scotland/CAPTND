
########################.
### MMI - discharges ###
########################.

# Author: Luke Taylor
# Date: 2026-01-05

summarise_discharges <- function(df){
  
  # create for for saving output files in
  dis_dir <- paste0(shorewise_pub_data_dir, "/discharges/")
  dir.create(dis_dir)
  measure_label <- "discharges_"
  
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    filter(!!sym(case_closed_month_o) %in% date_range &
             record_type_label == 'Discharge') |> # apply date range filter
    add_sex_description() |> 
    tidy_age_group_order() |>
    remove_borders_int_refs()
  
  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    #add_proportion_ds_hb() |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(dis_dir, measure_label, "all_hb"))
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_single_row |> 
    group_by(!!sym(case_closed_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(case_closed_month_o), !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    #add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    right_join(df_month_ds_hb, by = c("case_closed_month" = "month", "dataset_type", "hb_name")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           count = case_when(is.na(count) ~ 0,
                             TRUE ~ count)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(case_closed_month_o)) |> 
    save_as_parquet(path = paste0(dis_dir, measure_label, "month_hb")) |> 
    
    append_quarter_ending(date_col = "case_closed_month") |> 
    right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    #add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(dis_dir, measure_label, "quarter_hb"))
  
}

