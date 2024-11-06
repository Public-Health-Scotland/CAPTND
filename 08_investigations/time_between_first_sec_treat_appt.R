###################################################
#### Time between first and second appointment ####
###################################################

df <- read_parquet('../../../output/analysis_2024-09-25/swift_glob_completed_rtt.parquet') 

df_treat_wait <- df |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, rtt_eval_o, app_date_o, 
                   att_status_o, new_or_return_app_o, first_treat_app_o))) |>
  distinct() |>
  filter(!is.na(!!sym(first_treat_app_o))) |> #remove records without first_treat_app date
  arrange(!!sym(patient_id_o), !!sym(ucpn_o)) |>
  mutate(year = lubridate::year(!!sym(first_treat_app_o))) |>
  mutate(days_since_first_treat_appt = as.numeric(difftime(!!sym(app_date_o), !!sym(first_treat_app_o), units = 'days'))) |>
  arrange(!!sym(patient_id_o), !!sym(ucpn_o), days_since_first_treat_appt) |>
  filter(days_since_first_treat_appt >= 0 & #remove pre-treatment appt dates
           !is.na(days_since_first_treat_appt)) |> # remove rows without appt_date
  mutate(flag = case_when(!!sym(new_or_return_app_o) == 'new - treatment start' & 
                            !!sym(att_status_o) == 1 &
                            !!sym(first_treat_app_o) == !!sym(app_date_o) ~ 1,
                          !!sym(new_or_return_app_o) == 'return' &
                            !!sym(app_date_o) >= !!sym(first_treat_app_o) ~ 1,
                          TRUE ~ 0)) |>
  filter(flag == 1)

#Average length of time between first treatment appt and second appt in days, by year of first treatment appt
df_first_sec_treat_wait <- df_treat_wait |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 2) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, 
         !!sym(new_or_return_app_o), days_since_first_treat_appt) |>
  filter(days_since_first_treat_appt >= 0 &
           !!sym(new_or_return_app_o) == 'return') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(avg_wait = round(mean(days_since_first_treat_appt),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

#Scottish Average
scot_avg <- df_treat_wait |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 2) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, 
         !!sym(new_or_return_app_o), days_since_first_treat_appt) |>
  filter(days_since_first_treat_appt >= 0 &
           !!sym(new_or_return_app_o) == 'return') |>
  group_by(!!sym(dataset_type_o), year) |>
  mutate(avg_wait = round(mean(days_since_first_treat_appt),1)) |>
  ungroup() |>
  mutate(hb_name = 'NHSScotland') |>
  select(!!sym(dataset_type_o), hb_name, year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

df_first_sec_treat_wait <- rbind(df_first_sec_treat_wait, scot_avg)

##########################################################
##### Avg length of time between return appointment ######
##########################################################

#Average legnth of time between attended appts in days, by year of first treatment appt
df_ret_appt_wait <- df_treat_wait |>
  filter(!!sym(att_status_o) == 1) |>
  mutate(lag_col = lag(days_since_first_treat_appt)) |>
  mutate(days_between_appts = days_since_first_treat_appt - lag_col) |>
  filter(!!sym(new_or_return_app_o) == 'return') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(avg_wait = round(mean(days_between_appts),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

scot_avg <- df_treat_wait |>
  filter(!!sym(att_status_o) == 1) |>
  mutate(lag_col = lag(days_since_first_treat_appt)) |>
  mutate(days_between_appts = days_since_first_treat_appt - lag_col) |>
  filter(!!sym(new_or_return_app_o) == 'return') |>
  group_by(!!sym(dataset_type_o), year) |>
  mutate(avg_wait = round(mean(days_between_appts),1)) |>
  ungroup() |>
  mutate(hb_name = 'NHSScotland') |>
  select(!!sym(dataset_type_o), hb_name, year, avg_wait) |>
  arrange(!!sym(dataset_type_o), year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

df_ret_appt_wait <- rbind(df_ret_appt_wait, scot_avg)

#################################################################################################
##### Avg length of time between attended first assessment and first treatment appointment ######
#################################################################################################

df_assess_treat <- df |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, rtt_eval_o, app_date_o, 
                   att_status_o, app_purpose_o, new_or_return_app_o, first_treat_app_o))) |>
  distinct() |>
  filter(!is.na(!!sym(first_treat_app_o)) &
           !!sym(att_status_o) == 1) |> 
  arrange(!!sym(patient_id_o), !!sym(ucpn_o)) |>
  mutate(assess_date = case_when(!!sym(new_or_return_app_o) == 'new - pre treatment' ~ !!sym(app_date_o))) |>
  group_by(across(all_of(data_keys))) |>
  fill(assess_date, .direction="downup") |>
  ungroup() |>
  mutate(year = lubridate::year(assess_date)) |> # year of assessment appointment
  mutate(days_since_assess_appt = as.numeric(difftime(!!sym(first_treat_app_o), assess_date, units = 'days'))) |>
  filter(!is.na(days_since_assess_appt)) |>
  select(!!!syms(data_keys), year, days_since_assess_appt) |>
  group_by(!!!syms(data_keys)) |>
  filter(days_since_assess_appt == max(days_since_assess_appt)) |>
  distinct()

#Avg length of time between first attended assessment appt and first treatment appt in days, by year of assessment appt
df_assess_treat_hb <- df_assess_treat |>  
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(avg_wait = round(mean(days_since_assess_appt),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

scot_avg <- df_assess_treat |>
  group_by(!!sym(dataset_type_o), year) |>
  mutate(avg_wait = round(mean(days_since_assess_appt),1)) |>
  ungroup() |>
  mutate(hb_name = "NHSScotland") |>
  select(!!sym(dataset_type_o), hb_name, year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()
  
df_assess_treat <- rbind(df_assess_treat_hb, scot_avg)


create_time_frame_bar_chart <- function(df,
                                        ds = c("CAMHS", "PT"),
                                        year){
  
  df_graph <- df |> 
    ungroup() |> 
    filter(year == 2023 &
             !!sym(dataset_type_o) == ds)
  
  ggplot(df_graph, aes(x = fct_rev(!!sym(hb_name_o)), y = avg_wait, 
                       fill = factor(ifelse(hb_name == 'NHSScotland', "Highlighted", "Normal")))) +
    geom_bar(stat = "identity", width = 0.75) +
    scale_fill_manual(name = 'hb_name', values = c('#83BB26', '#9B4393')) +
    labs(
      title = paste0("Average number of days between first assessment appointment\nand first treatment appointment for ", ds," patients with a\nfirst assessment appointment in ", year, ", by health board"),
      x = "Health Board",
      y = "Average Wait (Days)",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    coord_flip() +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none",
          plot.margin = margin(r =15,
                               l = 5,
                               t = 10,
                               b = 10))
  
}

chart_height <- 14
chart_width <- 24

ggsave("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/assess_treat_appt_pt.png",
       bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
