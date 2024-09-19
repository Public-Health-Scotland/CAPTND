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

  # get appointments df
  df_app <- get_appointments_df() 
  
  # get first contact appointment date per pathway only
  df_first_app <- df_app |>
    arrange(!!ucpn_o, !!app_date_o) |> 
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    slice(1)|> 
    ungroup() |> 
    as.data.frame() |> 
    filter(!!sym(app_month_o) %in% date_range) |>
    mutate(Attendance = fcase(
      !!sym(att_status_o) == "1", "Attended",
      !!sym(att_status_o) == "2", "Clinic cancelled",
      !!sym(att_status_o) == "3", "Patient cancelled",
      !!sym(att_status_o) == "5", "Patient CNW",
      !!sym(att_status_o) == "8", "Patient DNA",
      !!sym(att_status_o) == "9", "Patient died",
      !!sym(att_status_o) == "99", "Not known",
      default = "Not recorded"),
      !!sym(age_group_o) := as.character(!!sym(age_group_o))) |> 
    add_sex_description()
  
  # get total apps for each time period
  #all time
  df_tot_app_all <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |>
    group_by(!!sym(dataset_type_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    ungroup()
  
  #quarterly
  df_tot_app_qt <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
    summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup()
  
  #monthly
  df_tot_app_mth <- df_app |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o)) |>  
    summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), !!sym(app_month_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))|> 
    ungroup()
  

  # 1. ALL TIME ---------------------------------------------------------
  
  # by hb - for supplement
  first_att_all <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance) |>
    summarise(firstcon_att = n(), .groups = 'drop') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |> 
    group_by(!!sym(dataset_type_o), Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
    left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
    arrange(!!dataset_type_o, !!hb_name_o, Attendance) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb"))
  
  
  # by hb and sex
  first_att_all_sex <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
             !!sym(sex_reported_o)) |>
    summarise(firstcon_att = n(), .groups = 'drop') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |> 
    group_by(!!sym(dataset_type_o), Attendance, !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
    left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, !!sex_reported_o) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb_sex"))
    
  
    # by hb and age
    first_att_all_age <- df_first_app |>
      group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
               !!sym(age_group_o)) |>
      summarise(firstcon_att = n(), .groups = 'drop') |>
      group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
      mutate(first_contact = sum(firstcon_att)) |> 
      ungroup() |> 
      group_by(!!sym(dataset_type_o), Attendance, !!sym(age_group_o)) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(!!sym(hb_name_o), ~"NHS Scotland"),
                          .groups = "drop")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
             prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
      left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
      arrange(!!dataset_type_o, !!hb_name_o, Attendance,
              readr::parse_number(!!sym(age_group_o))) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb_age"))
    
    
    # by hb and simd
    first_att_all_simd <- df_first_app |>
      group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
               !!sym(simd_quintile_o)) |>
      summarise(firstcon_att = n(), .groups = 'drop') |>
      group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
      mutate(first_contact = sum(firstcon_att)) |> 
      ungroup() |> 
      group_by(!!sym(dataset_type_o), Attendance, !!sym(simd_quintile_o)) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(!!sym(hb_name_o), ~"NHS Scotland"),
                          .groups = "drop")) |>
      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
             prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
      left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
      arrange(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
              !!sym(simd_quintile_o)) |>
      save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb_simd"))
  
  
  # 2. QUARTERLY -------------------------------------------------------
  
  # by hb and quarter - for presenting in supplement
  first_att_qt <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, Attendance) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |> 
    mutate(first_contact = sum(firstcon_att), 
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1),
           !!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d")) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    ungroup() |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb")) 
    
    
  # by hb, quarter, and sex - for presenting in supplement
  first_att_qt_sex <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, 
             Attendance, !!sym(sex_reported_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, Attendance, 
             !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, !!sym(sex_reported_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_month = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, 
            !!sym(sex_reported_o)) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_sex")) 

  
  # by hb, quarter, and age group - for presenting in supplement
  first_att_qt_age <- df_first_app |>
    mutate(!!sym(age_group_o) := as.character(!!sym(age_group_o))) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
             app_quarter_ending, !!sym(age_group_o)) |> 
    group_by(!!sym(dataset_type_o), Attendance, app_quarter_ending, !!sym(age_group_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, 
             !!sym(age_group_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_month = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(!!dataset_type_o, !!hb_name_o, Attendance, app_quarter_ending, 
            readr::parse_number(!!sym(age_group_o))) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_age")) 
  
  
  # by hb, quarter, and simd - for presenting in supplement
  first_att_qt_simd <- df_first_app |>
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
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_simd"))


  # 3. MONTHLY --------------------------------------------------------- 
  
  # by hb and month - for presenting in supplement
  first_att_mth <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), Attendance) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), !!sym(app_month_o), Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o)) |>
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           !!sym(app_month_o) := as.Date(!!sym(app_month_o), "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb")) 

  
  # by hb, month, and sex - for presenting in supplement
  first_att_mth_sex <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), Attendance, !!sym(sex_reported_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), !!sym(app_month_o), Attendance, !!sym(sex_reported_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), !!sym(sex_reported_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           !!sym(app_month_o) := as.Date(!!sym(app_month_o), "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o, !!sym(sex_reported_o)) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_sex")) 

  
  # by hb, month, and age - for presenting in supplement
  first_att_mth_age <- df_first_app |>
    mutate(age_group = as.character(age_group)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), Attendance, !!sym(age_group_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), !!sym(app_month_o), Attendance, !!sym(age_group_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), !!sym(age_group_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           !!sym(app_month_o) := as.Date(!!sym(app_month_o), "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o, readr::parse_number(!!sym(age_group_o))) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_age")) 
  
  
  # by hb, month, and simd - for presenting in supplement
  first_att_mth_simd <- df_first_app |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), Attendance, !!sym(simd_quintile_o)) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(!!sym(dataset_type_o), !!sym(app_month_o), Attendance, !!sym(simd_quintile_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(app_month_o), !!sym(simd_quintile_o)) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           !!sym(app_month_o) := as.Date(!!sym(app_month_o), "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o, !!simd_quintile_o) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_simd"))
}
