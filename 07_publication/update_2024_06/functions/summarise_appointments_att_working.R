##################################################.
#### APPOINTMENT ATTENDANCE - for publication ####.
##################################################.

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
    group_by(ucpn, patient_id, dataset_type, hb_name) |> 
    arrange(ucpn, app_date) |> 
    slice(1) |> 
    filter(app_month %in% date_range) |> 
    mutate(Attendance = case_when(
      att_status == "1" ~ "Attended",
      att_status == "2" ~ "Clinic cancelled",
      att_status == "3" ~ "Patient cancelled",
      att_status == "5" ~ "Patient CNW",
      att_status == "8" ~ "Patient DNA",
      att_status == "9" ~ "Patient died",
      att_status == "99" ~ "Not known",
      TRUE ~ "Not recorded"),
      age_group = as.character(age_group)) |> 
    add_sex_description()
  
  # get total apps for each time period
  #all time
  df_tot_app_all <- df_app |>
    filter(app_month %in% date_range) |>
    group_by(dataset_type, hb_name) |>
    summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |>
    group_by(dataset_type) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop"))|>
    ungroup()
  
  #quarterly
  df_tot_app_qt <- df_app |>
    filter(app_month %in% date_range) |> 
    group_by(dataset_type, hb_name, app_quarter_ending) |>  
    summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(dataset_type, app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup()
  
  #monthly
  df_tot_app_mth <- df_app |>
    filter(app_month %in% date_range) |> 
    group_by(dataset_type, hb_name, app_month) |>  
    summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(dataset_type, app_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop"))|> 
    ungroup()
  

  # 1. ALL TIME ---------------------------------------------------------
  
  # by hb - for supplement
  first_att_all <- df_first_app |>
    group_by(dataset_type, hb_name, Attendance) |>
    summarise(firstcon_att = n(), .groups = 'drop') |>
    group_by(dataset_type, hb_name) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |> 
    group_by(dataset_type, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
    left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
    arrange(dataset_type, hb_name, Attendance) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb"))
  
  
  # by hb and sex
  first_att_all_sex <- df_first_app |>
    group_by(dataset_type, hb_name, Attendance, sex_reported) |>
    summarise(firstcon_att = n(), .groups = 'drop') |>
    group_by(dataset_type, hb_name, sex_reported) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |> 
    group_by(dataset_type, Attendance, sex_reported) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
    left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
    arrange(dataset_type, hb_name, Attendance, sex_reported) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb_sex"))
    
  
    # by hb and age
    first_att_all_age <- df_first_app |>
      group_by(dataset_type, hb_name, Attendance, age_group) |>
      summarise(firstcon_att = n(), .groups = 'drop') |>
      group_by(dataset_type, hb_name, age_group) |> 
      mutate(first_contact = sum(firstcon_att)) |> 
      ungroup() |> 
      group_by(dataset_type, Attendance, age_group) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |>
      mutate(hb_name = factor(hb_name, levels = level_order_hb),
             prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
      left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
      arrange(dataset_type, hb_name, Attendance, readr::parse_number(age_group)) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb_age"))
    
    
    # by hb and simd
    first_att_all_simd <- df_first_app |>
      group_by(dataset_type, hb_name, Attendance, simd2020_quintile) |>
      summarise(firstcon_att = n(), .groups = 'drop') |>
      group_by(dataset_type, hb_name, simd2020_quintile) |> 
      mutate(first_contact = sum(firstcon_att)) |> 
      ungroup() |> 
      group_by(dataset_type, Attendance, simd2020_quintile) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(hb_name, ~"NHS Scotland"),
                          .groups = "drop")) |>
      mutate(hb_name = factor(hb_name, levels = level_order_hb),
             prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |> 
      left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
      arrange(dataset_type, hb_name, Attendance, simd2020_quintile) |>
      save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb_simd"))
  
  
  # 2. QUARTERLY -------------------------------------------------------
  
  # by hb and quarter - for presenting in supplement
  first_att_qt <- df_first_app |>
    group_by(dataset_type, hb_name, app_quarter_ending, Attendance) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, app_quarter_ending, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(dataset_type, hb_name, app_quarter_ending) |> 
    mutate(first_contact = sum(firstcon_att), 
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1),
           hb_name = factor(hb_name, levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d")) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb")) 
    
    
  # by hb, quarter, and sex - for presenting in supplement
  first_att_qt_sex <- df_first_app |>
    group_by(dataset_type, hb_name, app_quarter_ending, Attendance, sex_reported) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_quarter_ending, sex_reported) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    group_by(dataset_type, app_quarter_ending, Attendance, sex_reported) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending, sex_reported) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_sex")) 

  
  # by hb, quarter, and age group - for presenting in supplement
  first_att_qt_age <- df_first_app |>
    mutate(age_group = as.character(age_group)) |>
    group_by(dataset_type, hb_name, app_quarter_ending, Attendance, age_group) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_quarter_ending, age_group) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    group_by(dataset_type, app_quarter_ending, Attendance, age_group) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending, readr::parse_number(age_group)) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_age")) 
  
  
  # by hb, quarter, and simd - for presenting in supplement
  first_att_qt_simd <- df_first_app |>
    group_by(dataset_type, hb_name, app_quarter_ending, Attendance, simd2020_quintile) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_quarter_ending, simd2020_quintile) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    group_by(dataset_type, app_quarter_ending, Attendance, simd2020_quintile) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending, simd2020_quintile) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_simd"))


  # 3. MONTHLY --------------------------------------------------------- 
  
  # by hb and month - for presenting in supplement
  first_att_mth <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance) |>  
    summarise(firstcon_att = n()) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |> 
    group_by(dataset_type, app_month, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    arrange(dataset_type, hb_name, app_month) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb")) 

  
  # by hb, month, and sex - for presenting in supplement
  first_att_mth_sex <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance, sex_reported) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, sex_reported) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, sex_reported) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, sex_reported) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_sex")) 

  
  # by hb, month, and age - for presenting in supplement
  first_att_mth_age <- df_first_app |>
    mutate(age_group = as.character(age_group)) |>
    group_by(dataset_type, hb_name, app_month, Attendance, age_group) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, age_group) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, age_group) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, readr::parse_number(age_group)) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_age")) 
  
  
  # by hb, month, and simd - for presenting in supplement
  first_att_mth_simd <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance, simd2020_quintile) |>  
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, simd2020_quintile) |> 
    mutate(first_contact = sum(firstcon_att)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, simd2020_quintile) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_simd"))
}
