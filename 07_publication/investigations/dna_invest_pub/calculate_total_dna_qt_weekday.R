#####################################################################.
#### DNA Focused Publication - Total DNAs by Quarter and Weekday ####.
#####################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

total_appts_quarter_weekday <- function(df){
  
  # create for for saving output files in
  apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
  dir.create(apps_att_dir)
  
  # measure labels
  measure_label <- "total_dnas_" # for file names
  
  # by hb, quarter, and weekday - for presenting in supplement
  #Total Appts
  df_tot_app <- df |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    mutate(day_of_week = weekdays(app_date)) |>
    mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                        "Thursday", "Friday", "Saturday", "Sunday"))) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), day_of_week) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))
  
  #Total Appts by Quarter
  df_tot_app_qt <- read_parquet(paste0(apps_att_dir, "total_dnas_tot_appts_df.parquet")) |>
    filter(!!sym(app_month_o) %in% date_range) |> 
    mutate(day_of_week = weekdays(app_date)) |>
    mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                        "Thursday", "Friday", "Saturday", "Sunday"))) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, day_of_week) |>  
    summarise(total_apps = n(), .groups = 'drop') |> 
    group_by(!!sym(dataset_type_o), app_quarter_ending, day_of_week) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))
  
  
  df_app_qt_weekday <- read_parquet(paste0(apps_att_dir, "total_dnas_tot_appts_df.parquet")) |> 
    filter(!!sym(app_month_o) %in% date_range,
           !!sym(att_status_o) == "8") |>
    mutate(attendance = fcase(
      !!sym(att_status_o) == "8", "Patient DNA"),
      day_of_week = weekdays(app_date)) |>
    mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                        "Thursday", "Friday", "Saturday", "Sunday"))) |>
    filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week, app_quarter_ending) |>
    summarise(dna_count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), day_of_week, app_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "day_of_week", "app_quarter_ending")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
           app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),           
           prop_apps_att = round(dna_count/total_apps*100, 1)) |>
    arrange(!!dataset_type_o, !!hb_name_o, !!app_month_o)  |> 
    save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_all_hb"))
  
}


