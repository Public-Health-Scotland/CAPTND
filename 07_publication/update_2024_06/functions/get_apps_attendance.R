##################################################.
#### APPOINTMENT ATTENDANCE - for publication ####.
##################################################.


# UPDATED method to use first contact appointment for reporting attendance


get_apps_attendance <- function(){
  
  # measure label for file names
  measure_label <- "apps_att_"
  
  # load data 
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
   
  # get appointments df
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
  
  # by hb - not currently needed in pdf or supplement
  first_att_all <- df_first_app |>
    group_by(dataset_type, hb_name, Attendance) |>  
    summarise(n = n(), .groups = 'drop') |> 
    group_by(dataset_type, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    pivot_wider(names_from = Attendance, values_from = n) |>
    adorn_totals("col", name = "first_contact") |>
    left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period DO FOR ALL OUTPUTS
    mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> #,
    #`Percent Attended` = round(Attended/Total*100, 1),
    #`Percent Attended` = paste0(`Percent Attended`, "%"),
    #across(Attended:Total, ~prettyNum(., big.mark = ","))) |> #across(Attended:`Not known`, ~replace(., is.na(.), ".."))
    arrange(dataset_type, hb_name) #|> 
    #filter(dataset_type = dataset_choice) |> 
    #save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "all_hb")) # _", dataset_choice))
  
  # 2. QUARTERLY -------------------------------------------------------
  
  # by hb and quarter - for presenting in supplement
  first_att_qt <- df_first_app |>
    group_by(dataset_type, hb_name, app_quarter_ending, Attendance) |>  
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, app_quarter_ending, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    group_by(dataset_type, hb_name, app_quarter_ending) |> 
    mutate(first_contact = sum(n), 
           hb_name = factor(hb_name, levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d")) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_quarter_ending) #|>
    #filter(dataset_type = dataset_choice) |> 
    #save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "qt_hb")) # _", dataset_choice))

  
  # Latest quarter - for presenting in pdf
  first_att_latest <- first_att_qt |> 
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> 
    pivot_wider(names_from = Attendance, values_from = n) |> 
    filter(app_quarter_ending == max(app_quarter_ending)) |>
    select(dataset_type, hb_name, total_apps, first_contact, `Patient DNA`) |> 
    mutate(`Percent 1st Contact DNA` = round(`Patient DNA`/first_contact*100, 1),
           `Percent 1st Contact DNA` = paste0(`Percent 1st Contact DNA`, "%"),
           across(total_apps:`Patient DNA`, ~prettyNum(., big.mark = ","))) |> 
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "latest_qt")) # _", dataset_choice))
  
  
  # Latest quarter DNA rate by SIMD for NHS Scotland - for plotting
  first_dna_simd_latest <- df_first_app |> 

    group_by(dataset_type, app_quarter_ending, Attendance, simd2020_quintile) |> 
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, app_quarter_ending, simd2020_quintile) |> 
    mutate(first_contact = sum(n),
           Percent = round(n/first_contact*100, 1)) |> 
    ungroup() |>
    filter(Attendance == "Patient DNA",
           app_quarter_ending == max(app_quarter_ending)) 
    
  
  # DNA rate only - quarterly by hb - no longer needed
  # first_att_dna_qt <- first_att_qt |> 
  #   filter(Attendance == "Patient DNA") |> 
  #   mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
  #          app_quarter_ending = format(app_quarter_ending, format = "%b '%y"),
  #          Percent = round(n/first_contact*100, 1),
  #          Percent = paste0(Percent, "%")) |> 
  #   select(-c("Attendance", "n", "first_contact")) |> 
  #   pivot_wider(names_from = app_quarter_ending, values_from = "Percent") #|> 
  #filter(dataset_type = dataset_choice) |> 
  #save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "qt_dna_rate_hb")) # _", dataset_choice))
  
  
  # DNA rate only - quarterly by hb_REGION - for plotting
  first_att_dna_qt_region <- first_att_qt |> 
    add_hb_region() |> 
    filter(Attendance == "Patient DNA") |>
    rename(patient_dna = n) |> 
    group_by(dataset_type, hb_region, app_quarter_ending) |> 
    summarise_at(c("patient_dna", "first_contact"), sum) |> 
    mutate(Percent = round(patient_dna/first_contact*100, 1))
  
  
  # 3. MONTHLY --------------------------------------------------------- 
  
  # by hb and month - for presenting in supplement
  first_att_mth <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance) |>  
    summarise(n = n()) |> 
    mutate(first_contact = sum(n)) |> 
    ungroup() |> 
    group_by(dataset_type, app_month, Attendance) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           `Percent 1st Contact DNA` = round(n/first_contact*100, 1),
           `Percent 1st Contact DNA` = paste0(`Percent 1st Contact DNA`, "%"),
           across(n:first_contact, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb")) # _", dataset_choice))
  
  
  # by hb, month, and sex - for presenting in supplement
  first_att_mth_sex <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance, Sex) |>  
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, Sex) |> 
    mutate(first_contact = sum(n)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, Sex) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           `Percent 1st Contact DNA` = round(n/first_contact*100, 1),
           `Percent 1st Contact DNA` = paste0(`Percent 1st Contact DNA`, "%"),
           across(n:first_contact, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, Sex) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb_sex")) # _", dataset_choice))
  
  
  # by hb, month, and age - for presenting in supplement
  first_att_mth_age <- df_first_app |>
    mutate(age_group = as.character(age_group)) |>
    group_by(dataset_type, hb_name, app_month, Attendance, age_group) |>  
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, age_group) |> 
    mutate(first_contact = sum(n)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, age_group) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           `Percent 1st Contact DNA` = round(n/first_contact*100, 1),
           `Percent 1st Contact DNA` = paste0(`Percent 1st Contact DNA`, "%"),
           across(n:first_contact, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, readr::parse_number(age_group)) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb_age")) # _", dataset_choice))
  
  
  # by hb, month, and simd - for presenting in supplement
  first_att_mth_simd <- df_first_app |>
    group_by(dataset_type, hb_name, app_month, Attendance, simd2020_quintile) |>  
    summarise(n = n(), .groups = "drop") |> 
    group_by(dataset_type, hb_name, app_month, simd2020_quintile) |> 
    mutate(first_contact = sum(n)) |> 
    ungroup() |>  
    group_by(dataset_type, app_month, Attendance, simd2020_quintile) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           app_month = as.Date(app_month, "%Y-%m-%d"),           
           `Percent 1st Contact DNA` = round(n/first_contact*100, 1),
           `Percent 1st Contact DNA` = paste0(`Percent 1st Contact DNA`, "%"),
           across(n:first_contact, ~prettyNum(., big.mark = ","))) |>
    ungroup() |> 
    arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
    left_join(df_tot_app_mth, by = c("dataset_type", "hb_name", "app_month")) |> 
    #filter(dataset_type = dataset_choice) |> 
    save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "mth_hb_simd")) # _", dataset_choice))
  
  
  
  # 4. MAKE PLOTS --------------------------------------------------------
  
  make_dna_simd_plot <- function(dataset_choice){
    
    dna_simd_plot_data <- first_dna_simd_latest |> 
      filter(dataset_type == dataset_choice,
             !is.na(simd2020_quintile)) |> 
      mutate(simd2020_quintile = as.factor(simd2020_quintile))
    
    ggplot(dna_simd_plot_data, aes(x = simd2020_quintile, y = Percent, fill = simd2020_quintile)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_manual(values = phs_colors(c("phs-purple", "phs-magenta", "phs-blue", "phs-liberty", "phs-teal"))) +
      theme(legend.position = "none")

    
    #ggsave(paste0(shorewise_pub_data_dir, "/dna_simd_plot_last_qt_", dataset_choice, ".png"), height = 10, width = 13, units = "cm")
    
  }
  
  make_dna_trend_plot <- function(dataset_choice){

    dna_trend_plot_data <- first_att_dna_qt_region |> 
      ungroup() |> 
      filter(dataset_type == dataset_choice,
             !is.na(hb_region))
    
    dates <- dna_trend_plot_data |>
      select(app_quarter_ending) |>
      unique() |>
      pull()
    
    ggplot(dna_trend_plot_data, aes(x = app_quarter_ending, y = Percent, colour = hb_region)) +
      geom_line() +
      geom_point() +
      scale_x_date(labels = format(dates, "%b %Y"), breaks = dates) +
      theme_minimal() +
      scale_colour_manual(values = phs_colors(c("phs-purple", "phs-magenta", "phs-blue"))) +
      theme(legend.position = "bottom")
    
    
    #ggsave(paste0(shorewise_pub_data_dir, "/dna_rate_trend_region_", dataset_choice, ".png"), height = 10, width = 13, units = "cm")
    
  }
  
  make_dna_simd_plot("CAMHS")
  make_dna_simd_plot("PT")
  
  make_dna_trend_plot("CAMHS")
  make_dna_trend_plot("PT")
}