###################################################
#### Time between first and second appointment ####
###################################################

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

df_treat_wait <- df |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, rtt_eval_o, 
                   app_date_o, att_status_o, new_or_return_app_o, first_treat_app_o))) |>
  filter(!is.na(!!sym(first_treat_app_o))) |> #remove records without first_treat_app date
  arrange(!!sym(patient_id_o), !!sym(ucpn_o)) |>
  mutate(ref_year = lubridate::year(!!sym(ref_rec_date_opti_o))) |>
  mutate(days_since_first_treat_appt = as.numeric(difftime(!!sym(app_date_o), !!sym(first_treat_app_o), units = 'days'))) |>
  filter(days_since_first_treat_appt >= 0 & #remove pre-treatment appt dates
           !is.na(days_since_first_treat_appt)) |> # remove rows without appt_date
  group_by(!!!syms(data_keys)) |>
  arrange(ucpn, days_since_first_treat_appt) |>
  #filter(att_status == 1) |>
  slice_head(n = 2) |>
  ungroup()


#Average length of time between first treatment appt and second scheduled or attended appt in days, by ref rec year
df_first_sec_treat_wait <- df_treat_wait |>
  select(!!sym(ucpn_o),!!sym(dataset_type_o), !!sym(hb_name_o), ref_year, 
         !!sym(new_or_return_app_o), !!sym(att_status_o), days_since_first_treat_appt) |>
  group_by(!!sym(ucpn_o),!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  slice(2) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(avg_wait = round(mean(days_since_first_treat_appt),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

#Scottish Average
scot_avg <- df_treat_wait |>
  select(!!sym(ucpn_o),!!sym(dataset_type_o), !!sym(hb_name_o), ref_year, 
         !!sym(new_or_return_app_o), !!sym(att_status_o), days_since_first_treat_appt) |>
  group_by(!!sym(ucpn_o),!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  slice(2) |>
  group_by(!!sym(dataset_type_o), ref_year) |>
  mutate(avg_wait = round(mean(days_since_first_treat_appt),1)) |>
  ungroup() |>
  mutate(hb_name = 'NHSScotland') |>
  select(!!sym(dataset_type_o), hb_name, ref_year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

df_first_sec_treat_wait <- rbind(df_first_sec_treat_wait, scot_avg)

#####################################################################
##### Avg length of time between attended, return appointments ######
#####################################################################

#Average legnth of time between attended appts in days, by referral year
df_ret_appt_wait <- df |>
  filter(!!sym(att_status_o) == 1,
         !is.na(!!sym(first_treat_app_o)),
         !!sym(new_or_return_app_o) == 'return') |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, rtt_eval_o, 
                   app_date_o, att_status_o, new_or_return_app_o, first_treat_app_o))) |>
  arrange(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(app_date_o)) |>
  mutate(ref_year = lubridate::year(!!sym(ref_rec_date_opti_o)),
         days_since_first_treat_appt = as.numeric(difftime(!!sym(app_date_o), !!sym(first_treat_app_o), units = 'days'))) |>
  group_by(!!!syms(data_keys)) |>
  mutate(lag_col = lag(days_since_first_treat_appt),
         days_between_appts = days_since_first_treat_appt - lag_col) |>
  filter(!is.na(days_between_appts)) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(avg_wait = round(mean(days_between_appts),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()
  

#################################################################################################
##### Avg length of time between attended first assessment and first treatment appointment ######
#################################################################################################

df_assess_treat <- df |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, 
                   ref_rec_date_opti_o, rtt_eval_o, app_date_o, 
                   att_status_o, app_purpose_o, new_or_return_app_o, first_treat_app_o))) |>
  filter(!is.na(!!sym(first_treat_app_o)) &
           !!sym(att_status_o) == 1) |> 
  arrange(!!sym(ucpn_o), !!sym(app_date_o)) |>
  group_by(!!!syms(data_keys)) |>
  mutate(flag = case_when(!!sym(app_purpose_o) == 1 ~ 'Assessment',
                          !!sym(app_purpose_o) %in% c(2, 3, 5) ~ 'Treatment',
                          TRUE ~ NA)) |>
  filter(!is.na(flag)) |>
  mutate(assess_date = case_when(flag == 'Assessment' & app_date == min(app_date) ~ app_date,
                                 TRUE ~ NA_Date_)) |>
  filter(!is.na(assess_date)) |>
  fill(assess_date, .direction="downup") |>
  mutate(ref_year = lubridate::year(!!sym(ref_rec_date_opti_o)),
         days_since_assess_appt = as.numeric(difftime(!!sym(first_treat_app_o), assess_date, units = 'days'))) |>
  select(!!!syms(data_keys), ref_year, days_since_assess_appt) |>
  distinct() |>
  ungroup()

#Avg length of time between first attended assessment appt and first treatment appt in days, by year of assessment appt
df_assess_treat_hb <- df_assess_treat |>  
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(avg_wait = round(mean(days_since_assess_appt),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
  mutate(hb_name = as_factor(hb_name)) |>
  distinct()

scot_avg <- df_assess_treat |>
  group_by(!!sym(dataset_type_o), ref_year) |>
  mutate(avg_wait = round(mean(days_since_assess_appt),1)) |>
  ungroup() |>
  mutate(hb_name = "NHSScotland") |>
  select(!!sym(dataset_type_o), hb_name, ref_year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), ref_year) |>
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
