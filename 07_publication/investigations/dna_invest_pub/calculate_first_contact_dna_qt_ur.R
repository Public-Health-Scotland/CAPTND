#######################################################################################.
#### DNA Focused Publication - First Contact DNAs by Quarter and Urban Rural Class ####.
#######################################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

firstcon_appt_quarter_ur <- function(df){
  
  # create for for saving output files in
  apps_firstcon_dir <- paste0(shorewise_pub_data_dir, "/appointments_firstcon/")
  dir.create(apps_firstcon_dir)
  
  # measure labels
  measure_label <- "firstcon_dnas_" # for file names
  
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
  
  # by hb, quarter, and ur - for presenting in supplement
  first_att_qt_ur <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             ur8_2022_name) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    mutate(ur8_2022_name = case_when(is.na(ur8_2022_name) ~ 'Not known',
                                     TRUE ~ ur8_2022_name)) |>
    right_join(df_ur_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                    "ur8_2022_name", "app_quarter_ending")) |>
    mutate(firstcon_att = case_when(is.na(firstcon_att) ~ 0,
                                TRUE ~ firstcon_att)) |>
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, ur8_2022_name) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, ur8_2022_name) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            ur8_2022_name) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_ur"))
  
  
  #complete skeleton df
  df_ur_sex_mth_hb <- df_ds_hb_name |>
    cross_join(ur_class_df) |>
    cross_join(att_status_df) |>
    cross_join(qt_df) |>
    cross_join(sex_df) |>
    filter(hb_name != 'NHS Scotland')
  
  # by hb, quarter, ur and sex - for presenting in supplement
  first_att_qt_ur_sex <- df |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, app_quarter_ending, 
             ur8_2022_name, !!sym(sex_reported_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    mutate(ur8_2022_name = case_when(is.na(ur8_2022_name) ~ 'Not known',
                                     TRUE ~ ur8_2022_name),
           sex_reported = case_when(is.na(sex_reported) ~ 'Data missing',
                                    TRUE ~ sex_reported)) |>
    right_join(df_ur_sex_mth_hb, by = c("dataset_type", "hb_name", "Attendance" = "att_status",
                                        "ur8_2022_name", "sex_reported", "app_quarter_ending")) |>
    mutate(firstcon_att = case_when(is.na(firstcon_att) ~ 0,
                                TRUE ~ firstcon_att)) |>
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, ur8_2022_name,
             !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, ur8_2022_name,
             !!sym(sex_reported_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            ur8_2022_name, !!sex_reported_o) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    #filter(Attendance == 'Patient DNA') |> 
    save_as_parquet(paste0(apps_firstcon_dir, measure_label, "qt_hb_ur_sex"))
  
}




