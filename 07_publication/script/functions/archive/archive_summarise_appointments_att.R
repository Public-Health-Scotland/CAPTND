########################################################.
#### APPOINTMENT ATTENDANCE - for publication & mmi ####.
########################################################.

# Author: Bex Madden
# Date: 2024-07-15

# Uses first contact appointment for reporting attendance

summarise_appointments_att <- function(){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "apps_att_" # for file names
  
  demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group", "location", "prof_group")

  df_app <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    remove_borders_int_refs() |>
    filter(!!sym(app_month_o) %in% date_range) |>
    select(all_of(data_keys), !!app_date_o, !!app_month_o, !!att_status_o, all_of(demographics)) |> # need to account for multiples
    filter(!is.na(!!sym(app_date_o))) |> 
    mutate(Attendance = fcase(
      !!sym(att_status_o) == "1", "Attended",
      !!sym(att_status_o) == "2", "Clinic cancelled",
      !!sym(att_status_o) == "3", "Patient cancelled",
      !!sym(att_status_o) == "5", "Patient CNW",
      !!sym(att_status_o) == "8", "Patient DNA",
      !!sym(att_status_o) == "9", "Patient died",
      !!sym(att_status_o) == "99", "Not known",
      default = "Not recorded")) |>
    ungroup() |> 
    mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
           app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
           app_quarter_ending = floor_date(app_quarter, unit = "month"))
  
  
  # get appointments df
  # df_app <- get_appointments_df() |> 
  #   filter(!!sym(app_month_o) %in% date_range) |>
  #   mutate(Attendance = fcase(
  #     !!sym(att_status_o) == "1", "Attended",
  #     !!sym(att_status_o) == "2", "Clinic cancelled",
  #     !!sym(att_status_o) == "3", "Patient cancelled",
  #     !!sym(att_status_o) == "5", "Patient CNW",
  #     !!sym(att_status_o) == "8", "Patient DNA",
  #     !!sym(att_status_o) == "9", "Patient died",
  #     !!sym(att_status_o) == "99", "Not known",
  #     default = "Not recorded"),
  #     !!sym(age_group_o) := as.character(!!sym(age_group_o))) |> 
  #   add_sex_description()

  # get total apps for each time period
  # #all time
  # df_tot_app_all <- df_app |>
  #   filter(!!sym(app_month_o) %in% date_range) |>
  #   group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  #   summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |>
  #   group_by(!!sym(dataset_type_o)) %>%
  #   bind_rows(summarise(.,
  #                       across(where(is.numeric), sum),
  #                       across(!!sym(hb_name_o), ~"NHS Scotland"),
  #                       .groups = "drop")) |>
  #   ungroup()
  
  #quarterly
  df_tot_app_qt <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup()
  
  #monthly
  # df_tot_app_mth <- df_app |>
  #   filter(!!sym(app_month_o) %in% date_range) |> 
  #   group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o)) |>  
  #   summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
  #   group_by(!!sym(dataset_type_o), !!sym(app_month_o)) %>%
  #   bind_rows(summarise(.,
  #                       across(where(is.numeric), sum),
  #                       across(!!sym(hb_name_o), ~"NHS Scotland"),
  #                       .groups = "drop"))|> 
  #   ungroup()
  

  ### Total DNAs (not first contact) ---------------------------------------
  # Quarterly
  
  df_app_qt <- df_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_apps*100, 1)) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb"))
  
  
  # By sex
  
  df_app_qt_sex <- df_app |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance, !!sym(sex_reported_o)) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance, !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(sex_reported_o)) |> 
    mutate(total_sex = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_sex*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, app_quarter_ending) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_sex"))
  
  # By age
  
  df_app_qt_age <- df_app |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance, !!sym(age_group_o)) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance, !!sym(age_group_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(age_group_o)) |> 
    mutate(total_age = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_age*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_age"))
  
  # By simd
  
  df_app_qt_simd <- df_app |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance, !!sym(simd_quintile_o)) |>  
    summarise(apps_att = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance, !!sym(simd_quintile_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(simd_quintile_o)) |> 
    mutate(total_simd = sum(apps_att)) |> 
    ungroup() |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(apps_att/total_simd*100, 1)) |> 
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_simd"))
  
  
 }
