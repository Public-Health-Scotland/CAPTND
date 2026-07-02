###################################################################################.
#### DNA Focused Publication - First Contact DNAs by Quarter and SIMD Quintile ####.
###################################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_quarter_simd <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
  # by hb, quarter, and simd - for presenting in supplement
  first_att_qt_simd <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             !!sym(simd_quintile_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, !!sym(simd_quintile_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(simd_quintile_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            !!simd_quintile_o) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_simd"))
  
  # by hb, quarter, simd and sex - for presenting in supplement
  first_att_qt_simd_sex <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             !!sym(simd_quintile_o), !!sym(sex_reported_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, !!sym(simd_quintile_o),
             !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(simd_quintile_o),
             !!sym(sex_reported_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            !!simd_quintile_o, !!sex_reported_o) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_simd_sex"))
  
}



