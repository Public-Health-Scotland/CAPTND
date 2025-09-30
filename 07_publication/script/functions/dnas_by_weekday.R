#####################################
###### DNAs by Day of the Week ######
#####################################

#Author: Luke Taylor
#Date: 21/11/2024

#most recent quarter end
quarter_end_date <- ymd("2025/09/01")

# create for for saving output files in
apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
dir.create(apps_att_dir)

# measure labels
measure_label <- "dnas_" # for file names

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  remove_borders_int_refs() |>
  
  mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
         app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(app_quarter, unit = "month"))

# appt df with key variables
df_app <- df |>
  select(all_of(data_keys), !!app_date_o, !!app_month_o, !!att_status_o, !!att_cat_o, !!app_purpose_o,
         !!ref_acc_o, !!app_date_o, app_quarter_ending) |> 
  filter(!is.na(!!sym(app_date_o))) 

df_app_att <- df_app |>
  filter(!!sym(app_month_o) %in% date_range) |>
  mutate(Attendance = fcase(
    !!sym(att_status_o) == "1", "Attended",
    !!sym(att_status_o) == "2", "Clinic cancelled",
    !!sym(att_status_o) == "3", "Patient cancelled",
    !!sym(att_status_o) == "5", "Patient CNW",
    !!sym(att_status_o) == "8", "Patient DNA",
    !!sym(att_status_o) == "9", "Patient died",
    !!sym(att_status_o) == "99", "Not known",
    default = "Not recorded"))

#Total Appts
df_tot_app <- df_app_att |>
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
df_tot_app_qt <- df_app |>
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


#All DNAs by Quarter
# df_all_dnas <- df_app |> 
#   filter(!!sym(app_month_o) %in% date_range,
#          !!sym(att_status_o) == "8") |>
#   mutate(attendance = fcase(
#     !!sym(att_status_o) == "8", "Patient DNA"),
#     day_of_week = weekdays(app_date)) |>
#   mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
#                                                       "Thursday", "Friday", "Saturday", "Sunday"))) |>
#   filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
#   group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, day_of_week) |>
#   summarise(dna_count = n(), .groups = "drop") |>
#   group_by(!!sym(dataset_type_o), app_quarter_ending, day_of_week) %>%
#   bind_rows(summarise(.,
#                       across(where(is.numeric), sum),
#                       across(!!sym(hb_name_o), ~"NHS Scotland"),
#                       .groups = "drop")) |>
#   left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending", "day_of_week")) |>
#   save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_qr_hb")) |>
#   
#   group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
#   summarise(dna_count = sum(dna_count),
#             total_apps = sum(total_apps)) |>
#   save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_all_hb"))

#All DNAs by publication period
df_all_dnas <- df_app |> 
    filter(!!sym(app_month_o) %in% date_range,
           !!sym(att_status_o) == "8") |>
    mutate(attendance = fcase(
      !!sym(att_status_o) == "8", "Patient DNA"),
      day_of_week = weekdays(app_date)) |>
    mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                        "Thursday", "Friday", "Saturday", "Sunday"))) |>
    filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
    summarise(dna_count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), day_of_week) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    left_join(df_tot_app, by = c("dataset_type", "hb_name", "day_of_week")) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_all_hb"))
  

ds <- 'PT'

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
  
  
  ggsave(paste0(apps_att_dir, "dna_by_weekday_all_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}
  

############################## CANCELLATIONS ###################################

measure_label <- "cnas_"

#All Patient CNAs by Quarter
# df_all_cnas <- df_app |> 
#   filter(!!sym(app_month_o) %in% date_range,
#          !!sym(att_status_o) == "3") |>
#   mutate(attendance = fcase(
#     !!sym(att_status_o) == "3", "Patient CNA"),
#     day_of_week = weekdays(app_date)) |>
#   mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
#                                                       "Thursday", "Friday", "Saturday", "Sunday"))) |>
#   filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
#   group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending, day_of_week) |>
#   summarise(cna_count = n(), .groups = "drop") |>
#   group_by(!!sym(dataset_type_o), app_quarter_ending, day_of_week) %>%
#   bind_rows(summarise(.,
#                       across(where(is.numeric), sum),
#                       across(!!sym(hb_name_o), ~"NHS Scotland"),
#                       .groups = "drop")) |>
#   left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending", "day_of_week")) |>
#   save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_qr_hb")) |>
#   
#   group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
#   summarise(cna_count = sum(cna_count),
#             total_apps = sum(total_apps)) |>
#   save_as_parquet(path = paste0(apps_att_dir, measure_label, "weekday_all_hb"))

df_all_cnas <- df_app |> 
  filter(!!sym(app_month_o) %in% date_range,
         !!sym(att_status_o) == "3") |>
  mutate(attendance = fcase(
    !!sym(att_status_o) == "3", "Patient CNA"),
    day_of_week = weekdays(app_date)) |>
  mutate(day_of_week = factor(day_of_week, levels = c("Monday", "Tuesday", "Wednesday",
                                                      "Thursday", "Friday", "Saturday", "Sunday"))) |>
  filter(day_of_week != "Saturday" & day_of_week != "Sunday") |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
  summarise(cna_count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), day_of_week) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app, by = c("dataset_type", "hb_name", "day_of_week")) |>
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

  