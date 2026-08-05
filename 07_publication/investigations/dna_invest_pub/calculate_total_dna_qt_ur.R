###############################################################################.
#### DNA Focused Publication - Total DNAs by Quarter and Urban Rural Class ####.
###############################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

total_appts_quarter_ur <- function(df){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  #skeleton dataframes
  ur_class_df <- data.frame(ur8_2022_name = c("1 Large Urban Areas", "2 Other Urban Areas", "3 Accessible Small Towns",
                                              "4 Remote Small Towns", "5 Very Remote Small Towns", "6 Accessible Rural",
                                              "7 Remote Rural", "8 Very Remote Rural", "Not known"))
  
  att_status_df <- data.frame(att_status = c("Attended", "Clinic cancelled", "Patient DNA", "Patient cancelled",
                                             "Patient CNW", "Not known", "Not recorded"))
  
  sex_df <- data.frame(sex_reported = c("Male", "Female", "Not known", "Data missing"))
  
  qt_df <- df |>
    select(app_quarter_ending) |> distinct()
  
  #complete skeleton df
  df_ur_mth_hb <- df_ds_hb_name |>
    cross_join(ur_class_df) |>
    cross_join(att_status_df) |>
    cross_join(qt_df) |>
    filter(hb_name != 'NHS Scotland')
  
  # by hb, quarter, and urban v rural - for presenting in supplement
  df_app_qt_ur <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             ur8_2022_name) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    mutate(ur8_2022_name = case_when(is.na(ur8_2022_name) ~ 'Not known',
                                     TRUE ~ ur8_2022_name)) |>
    right_join(df_ur_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                    "ur8_2022_name", "app_quarter_ending")) |>
    mutate(apps_att = case_when(is.na(apps_att) ~ 0,
                                TRUE ~ apps_att)) |>
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, ur8_2022_name) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, ur8_2022_name) |> 
    mutate(total_ur = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_ur*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_ur"))
  
  #complete skeleton df
  df_ur_sex_mth_hb <- df_ds_hb_name |>
    cross_join(ur_class_df) |>
    cross_join(att_status_df) |>
    cross_join(qt_df) |>
    cross_join(sex_df) |>
    filter(hb_name != 'NHS Scotland')
  
  # by hb, quarter, urban v rural and sex - for presenting in supplement
  df_app_qt_ur_sex <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             ur8_2022_name, !!sym(sex_reported_o)) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    mutate(ur8_2022_name = case_when(is.na(ur8_2022_name) ~ 'Not known',
                                     TRUE ~ ur8_2022_name),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported)) |>
    right_join(df_ur_sex_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                    "ur8_2022_name", "sex_reported", "app_quarter_ending")) |>
    mutate(apps_att = case_when(is.na(apps_att) ~ 0,
                                TRUE ~ apps_att)) |>
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, ur8_2022_name,
             !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, ur8_2022_name,
             !!sym(sex_reported_o)) |> 
    mutate(total_ur = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_ur*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    #filter(Attendance == 'Patient DNA') |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_ur_sex"))
  
}



