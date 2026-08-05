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
  
  #skeleton dataframes
  simd_df <- data.frame(simd2020_quintile = c('1','2','3','4','5','Not known'))
  
  att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                             "Patient CNW", "Not known", "Not recorded"))
  
  sex_df <- data.frame(sex_reported = c("Male", "Female", "Not known", "Data missing"))
  
  qt_df <- df |>
    select(app_quarter_ending) |> distinct()
  
  #complete skeleton df
  df_simd_mth_hb <- df_ds_hb_name |>
    cross_join(simd_df) |>
    cross_join(att_status_df) |>
    cross_join(qt_df) |>
    filter(hb_name != 'NHS Scotland')
  
  # by hb, quarter, and simd - for presenting in supplement
  first_att_qt_simd <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             !!sym(simd_quintile_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |>
    mutate(simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Not known',
                                         TRUE ~ simd2020_quintile)) |>
    right_join(df_simd_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                      "simd2020_quintile", "app_quarter_ending")) |>
    mutate(firstcon_att = case_when(is.na(firstcon_att) ~ 0,
                                TRUE ~ firstcon_att)) |>
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, !!sym(simd_quintile_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(simd_quintile_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            !!simd_quintile_o) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_simd"))
  
  #complete skeleton df
  df_simd_sex_mth_hb <- df_ds_hb_name |>
    cross_join(simd_df) |>
    cross_join(att_status_df) |>
    cross_join(sex_df) |>
    cross_join(qt_df) |>
    filter(hb_name != 'NHS Scotland')
  
  # by hb, quarter, simd and sex - for presenting in supplement
  first_att_qt_simd_sex <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             !!sym(simd_quintile_o), !!sym(sex_reported_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    mutate(simd2020_quintile = as.character(simd2020_quintile),
           simd2020_quintile = case_when(is.na(simd2020_quintile) ~ 'Not known',
                                         TRUE ~ simd2020_quintile),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported)) |>
    right_join(df_simd_sex_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                      "simd2020_quintile", "sex_reported", "app_quarter_ending")) |>
    mutate(firstcon_att = case_when(is.na(firstcon_att) ~ 0,
                                    TRUE ~ firstcon_att)) |>
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
    mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            !!simd_quintile_o, !!sex_reported_o) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_simd_sex"))
  
}



