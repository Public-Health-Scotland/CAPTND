##################################################.
#### APPOINTMENT ATTENDANCE - for publication ####.
##################################################.


# UPDATED method to use first contact appointment for reporting attendance


get_apps_attendance <- function(){
  
  # measure label for file names
  measure_label <- "apps_first_att_"
  
  # load data 
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
   
  # get appointments df
  source("./07_publication/update_2024_06/functions/get_appointments_df.R")
  df_app <- get_appointments_df(df) 
  
  # get first appointment date per pathway only
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
  ),
  Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) 
  
  # get total apps for each time period
  #all time
  df_tot_app_all <- df_app |>
    filter(app_month %in% date_range) |> 
    group_by(dataset_type, hb_name) |>  
    summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(dataset_type) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHSScotland"),
                        .groups = "drop"))
  
  #quarterly
  df_tot_app_qt <- df_app |>
    filter(app_month %in% date_range) |> 
    group_by(dataset_type, hb_name, app_quarter_ending) |>  
    summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(dataset_type, app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHSScotland"),
                        .groups = "drop"))
  
  #monthly
  df_tot_app_mth <- df_app |>
    filter(app_month %in% date_range) |> 
    group_by(dataset_type, hb_name, app_month) |>  
    summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
    group_by(dataset_type, app_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHSScotland"),
                        .groups = "drop"))
  
  # get attendance status proportions for first contact apps
  
  # 1. ALL TIME ---------------------------------------------------------
  
  # by hb - not currently needed in pdf or supplement
  first_att_all <- df_first_app |>
    group_by(dataset_type, hb_name, Attendance) |>  
    summarise(appointments = n(), .groups = 'drop') |> 
    group_by(dataset_type, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    pivot_wider(names_from = Attendance, values_from = appointments) |>
    adorn_totals("col") |>
    left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
    mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> #,
    #`Percent Attended` = round(Attended/Total*100, 1),
    #`Percent Attended` = paste0(`Percent Attended`, "%"),
    #across(Attended:Total, ~prettyNum(., big.mark = ","))) |> #across(Attended:`Not known`, ~replace(., is.na(.), ".."))
    arrange(dataset_type, hb_name) |> 
    #filter(dataset_type = dataset_choice) |> 
    #save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "all_hb")) # _", dataset_choice))
  
  # 2. QUARTERLY -------------------------------------------------------
  
  # by hb and quarter - for presenting in supplement
  first_att_qt <- df_first_app |>
    group_by(dataset_type, hb_name, app_quarter_ending, Attendance) |>  
    summarise(Appointments = n()) |> 
    mutate(Total = sum(Appointments)) |> 
    ungroup() |> 
    group_by(dataset_type, app_quarter_ending, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
           Percent = round(Appointments/Total*100, 1),
           Percent = paste0(Percent, "%"),
           across(Appointments:Total, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending) |>
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "qt_hb")) # _", dataset_choice))
  
  # DNA rate only - for presenting in pdf
  first_att_dna_qt <- first_att_qt |> 
    filter(Attendance == "Patient DNA") |> 
    select(-c("Attendance", "Appointments", "Total")) |> 
    mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
           app_quarter_ending = format(app_quarter_ending, format = "%b '%y")) |> 
    pivot_wider(names_from = app_quarter_ending, values_from = "Percent") |> 
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "qt_dna_rate")) # _", dataset_choice))
  
  
  # 3. MONTHLY --------------------------------------------------------- 
  
  # by hb and month - for presenting in supplement
  first_att_mth <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance) |>  
    summarise(Appointments = n()) |> 
    mutate(Total = sum(Appointments)) |> 
    ungroup() |> 
    group_by(dataset_type, app_month, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           Percent = round(Appointments/Total*100, 1),
           Percent = paste0(Percent, "%"),
           across(Appointments:Total, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month) |>
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb")) # _", dataset_choice))
  
  
  # by hb, month, and sex - for presenting in supplement
  first_att_mth_sex <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance, Sex) |>  
    summarise(Appointments = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, Sex) |> 
    mutate(Total = sum(Appointments)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, Sex) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           Percent = round(Appointments/Total*100, 1),
           Percent = paste0(Percent, "%"),
           across(Appointments:Total, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, Sex) |>
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb_sex")) # _", dataset_choice))
  
  
  # by hb, month, and age - for presenting in supplement
  first_att_mth_age <- df_first_app |>
    mutate(age_group = as.character(age_group)) |>
    group_by(dataset_type, hb_name, app_month, Attendance, age_group) |>  
    summarise(Appointments = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, age_group) |> 
    mutate(Total = sum(Appointments)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, age_group) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           Percent = round(Appointments/Total*100, 1),
           Percent = paste0(Percent, "%"),
           across(Appointments:Total, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, readr::parse_number(age_group)) |>
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb_age")) # _", dataset_choice))
  
  
  # by hb, month, and simd - for presenting in supplement
  first_att_mth_simd <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance, simd2020_quintile) |>  
    summarise(Appointments = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, simd2020_quintile) |> 
    mutate(Total = sum(Appointments)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, simd2020_quintile) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           Percent = round(Appointments/Total*100, 1),
           Percent = paste0(Percent, "%"),
           across(Appointments:Total, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb_simd")) # _", dataset_choice))
  
}