#####################################
###### DNAs by Day of the Week ######
#####################################

#Author: Luke Taylor
#Date: 21/11/2024

#most recent quarter end
quarter_end_date <- ymd("2024/09/01")

# create for for saving output files in
apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
dir.create(apps_att_dir)

# measure labels
measure_label <- "dnas_" # for file names

# get appointments df
df_app <- get_appointments_df() 

#Total Appts
df_tot_app <- df_app |>
  filter(!!sym(app_month_o) %in% date_range) |> 
  mutate(day_of_week = weekdays(app_date)) |>
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday"))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>  
  summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(!!sym(dataset_type_o), day_of_week) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop"))

#Total Appts by Quarter
df_tot_app_qt <- df_app |>
  filter(!!sym(app_month_o) %in% date_range) |> 
  mutate(day_of_week = weekdays(app_date)) |>
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday"))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, day_of_week) |>  
  summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(!!sym(dataset_type_o), app_quarter_ending, day_of_week) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop"))


#All DNAs by Quarter
df_all_dnas <- df_app |> 
  filter(!!sym(app_month_o) %in% date_range,
         !!sym(att_status_o) == "8") |>
  mutate(attendance = fcase(
    !!sym(att_status_o) == "8", "Patient DNA"),
    day_of_week = weekdays(app_date)) |>
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday"))) |>
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, day_of_week) |>
  summarise(dna_count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), app_quarter_ending, day_of_week) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending", "day_of_week")) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_qr_hb")) |>
  
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
  summarise(dna_count = sum(dna_count),
            total_apps = sum(total_apps)) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_all_hb"))
  

ds <- 'CAMHS'

create_bar_chart_dna_weekday <- function(dataset_choice){
  
  df_all_dnas <- read_parquet(paste0(apps_att_dir, "dnas_weekday_all_hb.parquet")) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland",
           !!sym(dataset_type_o) == ds) |>
    mutate(dna_rate = round(dna_count/total_apps*100, 1))
  
  lims = round_any(max(df_all_dnas$dna_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(df_all_dnas, aes(x = day_of_week, y = dna_rate, fill = day_of_week)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(dna_rate, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_discrete_phs() +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Day of the Week",
      y = "DNA Rate (%)",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  
  #ggsave(paste0(apps_att_dir, "dna_simd_plot_last_qt_", dataset_choice, ".png"),
         #bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}
  

############################## CANCELLATIONS ###################################

measure_label <- "cnas_"

#All Patient CNAs by Quarter
df_all_cnas <- df_app |> 
  filter(!!sym(app_month_o) %in% date_range,
         !!sym(att_status_o) == "3") |>
  mutate(attendance = fcase(
    !!sym(att_status_o) == "3", "Patient CNA"),
    day_of_week = weekdays(app_date)) |>
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday"))) |>
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, day_of_week) |>
  summarise(cna_count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), app_quarter_ending, day_of_week) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending", "day_of_week")) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_qr_hb")) |>
  
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
  summarise(cna_count = sum(cna_count),
            total_apps = sum(total_apps)) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_all_hb"))


create_bar_chart_dna_weekday <- function(dataset_choice){
  
  df_all_cnas <- read_parquet(paste0(apps_att_dir, "cnas_weekday_all_hb.parquet")) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland",
           !!sym(dataset_type_o) == ds) |>
    mutate(cna_rate = round(cna_count/total_apps*100, 1))
  
  lims = round_any(max(df_all_cnas$cna_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(df_all_cnas, aes(x = day_of_week, y = cna_rate, fill = day_of_week)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(cna_rate, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_discrete_phs() +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Day of the Week",
      y = "CNA Rate (%)",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
}

  