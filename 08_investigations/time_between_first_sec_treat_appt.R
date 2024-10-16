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
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, days_since_first_treat_appt) |>
  filter(days_since_first_treat_appt > 0) |>
  #group_by(!!sym(dataset_type_o), year) |>
  #bind_rows(summarise(., 
                      #across(where(is.numeric), sum),
                      #across(!!hb_name_o, ~ 'NHS Scotland'),
                      #.groups = "drop")) |>
  mutate(avg_wait = round(mean(days_since_first_treat_appt),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  distinct()

##########################################################
##### Avg length of time between return appointment ######
##########################################################

#Average legnth of time between appts in days, by year of first treatment appt
df_ret_appt_wait <- df_treat_wait |>
  mutate(lag_col = lag(days_since_first_treat_appt)) |>
  mutate(days_between_appts = days_since_first_treat_appt - lag_col) |>
  filter(days_between_appts >= 0) |>
  filter(!!sym(new_or_return_app_o) == 'return') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(avg_wait = round(mean(days_between_appts),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  distinct()

##################################################################################
##### Avg length of time between assessment and first treatment appointment ######
##################################################################################

df_assess_treat <- df |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, rtt_eval_o, app_date_o, 
                   att_status_o, app_purpose_o, new_or_return_app_o, first_treat_app_o))) |>
  distinct() |>
  filter(!is.na(!!sym(first_treat_app_o))) |> #remove records without first_treat_app date
  arrange(!!sym(patient_id_o), !!sym(ucpn_o)) |>
  mutate(assess_date = case_when(!!sym(new_or_return_app_o) == 'new - pre treatment' ~ !!sym(app_date_o))) |>
  group_by(across(all_of(data_keys))) |>
  fill(assess_date, .direction="downup") |>
  ungroup() |>
  mutate(year = lubridate::year(assess_date)) |> # year of assessment appointment
  mutate(days_since_assess_appt = as.numeric(difftime(!!sym(first_treat_app_o), !!sym(app_date_o), units = 'days'))) |>
  arrange(!!sym(patient_id_o), !!sym(ucpn_o), !!sym(app_date_o)) |>
  filter(days_since_assess_appt >= 0 & #remove return appt dates
           !is.na(days_since_assess_appt)) |># remove rows with NAs
  mutate(flag = case_when(!!sym(new_or_return_app_o) == 'new - treatment start' &
                            !!sym(att_status_o) == 1 &
                            !!sym(first_treat_app_o) == !!sym(app_date_o) ~ 1, #remove multiple first appt rows
                          !!sym(new_or_return_app_o) == 'new - pre treatment' & 
                            !!sym(app_purpose_o) == 1 &
                            !!sym(att_status_o) == 1 &
                            !!sym(app_date_o) <= !!sym(first_treat_app_o) ~ 1, #keep first attended assessment appt
                          TRUE ~ 0)) |>
  filter(flag == 1)


#Average length of time between assessment and first treatment appt in days, by year of assessment appt
df_assess_treat_sum <- df_assess_treat |>
  group_by(!!!syms(data_keys)) |>
  filter(!!sym(new_or_return_app_o) == 'new - pre treatment' &
           days_since_assess_appt == max(days_since_assess_appt)) |>
  ungroup() |>
  #group_by(!!sym(dataset_type_o), year) |>
  #bind_rows(summarise(., 
                      #across(days_since_assess_appt, sum),
                      #across(!!hb_name_o, ~ 'NHS Scotland')))
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  mutate(avg_wait = round(mean(days_since_assess_appt),1)) |>
  ungroup() |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), year, avg_wait) |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), year) |>
  distinct()


create_bar_chart_non_acceptance_action <- function(df,
                                                   ds = c("CAMHS", "PT"),
                                                   year){
  
  df_graph <- df |> 
    ungroup() |> 
    filter(year == max(year) &
             !!sym(dataset_type_o) == ds)
  
  ggplot(df_graph, aes(x = !!sym(hb_name_o), y = avg_wait, fill = !!sym(dataset_type_o))) +
    geom_bar(stat = "identity", width = 0.75) +
    scale_fill_discrete_phs(palette = 2) +
    labs(
      title = paste0("Average number of days between assessment and first treatment appointment\n for ", ds, 
                     " patients, by health board and year of assessment"),
      x = "Health Board",
      y = "Average Wait (Days)",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    coord_flip() +
    #scale_x_reverse() +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
}

