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

  # load data 
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  # get appointments df
  df_app <- get_appointments_df(df) 
  
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
      TRUE ~ "Not recorded"
    )) |> 
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
  
  # get attendance status proportions for first contact apps
  
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
    #filter(dataset_type = dataset_choice) |>
    save_as_parquet(paste0(apps_att_dir, measure_label, "all_hb")) # _", dataset_choice))
  
  
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
    #filter(dataset_type = dataset_choice) |>
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
    #filter(dataset_type = dataset_choice) |>
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
      #filter(dataset_type = dataset_choice) |>
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
           prop_firstcon_dna = round(firstcon_att/first_contact*100, 1),
           hb_name = factor(hb_name, levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d")) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending) |>
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb")) # _", dataset_choice))
  
  
  ### Latest quarter - for presenting in pdf ----
  first_att_latest <- first_att_qt |> 
    select(-prop_firstcon_dna) |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    pivot_wider(names_from = Attendance, values_from = n) |> 
    filter(app_quarter_ending == max(app_quarter_ending)) |>
    select(dataset_type, hb_name, total_apps, first_contact, `Patient DNA`) |> 
    mutate(`1st contact DNA rate` = round(`Patient DNA`/first_contact*100, 1),
           `1st contact DNA rate` = paste0(`1st contact DNA rate`, "%"),
           across(total_apps:`Patient DNA`, ~prettyNum(., big.mark = ","))) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> 
    arrange(dataset_type, hb_name) |> 
    rename(`Health board` = hb_name,
           `Total appointments` = total_apps,
           `1st contact appointments` = first_contact,
           `1st contact DNA` = `Patient DNA`) |>
    filter(!is.na(`Health board`))  # remove empty nhs 24 row
    #filter(dataset_type = dataset_choice) |> 
    
    first_att_latest[is.na(first_att_latest)] <- ".." # make NAs ..
    save_as_parquet(first_att_latest, paste0(apps_att_dir, "table_", measure_label, "latest_qt")) # _", dataset_choice)) 
  
  
  ### Latest quarter DNA rate by SIMD for NHS Scotland - for plotting ----
  first_dna_simd_latest <- df_first_app |> 
    group_by(dataset_type, app_quarter_ending, Attendance, simd2020_quintile) |> 
    summarise(firstcon_att = n(), .groups = "drop") |> 
    group_by(dataset_type, app_quarter_ending, simd2020_quintile) |> 
    mutate(first_contact = sum(firstcon_att),
           Percent = round(n/first_contact*100, 1)) |> 
    ungroup() |>
    filter(Attendance == "Patient DNA",
           app_quarter_ending == max(app_quarter_ending)) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "latest_qt_DNA_forplot"))
  

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
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb")) # _", dataset_choice))
  
  
  ### DNA rate only - monthly by hb_REGION - for plotting ----
  first_att_dna_mth_region <- first_att_mth |> 
    add_hb_region() |> 
    filter(!is.na(hb_region),
           Attendance == "Patient DNA") |>
    rename(patient_dna = firstcon_att) |> 
    group_by(dataset_type, hb_region, app_month) |> 
    summarise_at(c("patient_dna", "first_contact"), sum) |> 
    mutate(Percent = round(patient_dna/first_contact*100, 1)) #|> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_DNA_rate_forplot"))
  
  
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
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_sex")) |>  # _", dataset_choice))
  
    #quarterly
    append_quarter_ending(date_col = "app_month") |>
    group_by(dataset_type, hb_name, quarter_ending, Attendance, sex_reported) |> 
    summarise_at(c("firstcon_att", "first_contact"), sum) |> 
    mutate(prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "quarter_ending" = "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_sex")) 
  
  
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
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_age")) |>  # _", dataset_choice))
  
  # quarterly
    append_quarter_ending(date_col = "app_month") |>
    group_by(dataset_type, hb_name, quarter_ending, Attendance, age_group) |> 
    summarise_at(c("firstcon_att", "first_contact"), sum) |> 
    mutate(prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "quarter_ending" = "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_age")) 
  
  
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
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "mth_hb_simd")) |>  # _", dataset_choice))

  # quarterly
  append_quarter_ending(date_col = "app_month") |>
    group_by(dataset_type, hb_name, quarter_ending, Attendance, simd2020_quintile) |> 
    summarise_at(c("firstcon_att", "first_contact"), sum) |> 
    mutate(prop_firstcon_att = round(firstcon_att/first_contact*100, 1)) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "quarter_ending" = "app_quarter_ending")) |> 
    save_as_parquet(paste0(apps_att_dir, measure_label, "qt_hb_simd")) 

}
