###################################################
#### Time between first and second appointment ####
###################################################

#Author: Luke Taylor
#Date: 23/02/2026

#time between first and second attended appt
df_treat_wait_att <- df |>
  filter(record_type_label == 'Appointment',
         !!sym(att_status_o) == 1) |> #attended appts only
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, 
                   app_date_o, att_status_o, app_purpose_o, case_closed_date_o))) |>
  append_quarter_ending(date_col = "app_date") |>
  group_by(!!!syms(data_keys)) |>
  mutate(count = n()) |>
  filter(count >= 2) |>
  arrange(!!sym(ucpn_o), !!sym(app_date_o)) |>
  slice_head(n = 2) |> #keep first two attended appts
  mutate(first_contact = min(app_date),
         days_between_appts = as.numeric(difftime(!!sym(app_date_o), first_contact, units = 'days'))) |> #calculate time between appts
  slice(2) |> #keep second appt only
  filter(quarter_ending %in% date_range) |> #only keep second appts within publication period
  ungroup()

df_hb_att_avg <- df_treat_wait_att |>
  group_by(dataset_type, hb_name, quarter_ending) |>
  summarise(avg_days = round(mean(days_between_appts), 1))


#time between first attended and second offered appt
df_treat_wait_offered <- df |>
  filter(record_type_label == 'Appointment') |>
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, 
                   app_date_o, att_status_o, app_purpose_o, case_closed_date_o))) |>
  append_quarter_ending(date_col = "app_date") |>
  group_by(!!!syms(data_keys)) |>
  arrange(!!sym(ucpn_o), !!sym(app_date_o)) |>
  mutate(first_attended_date = min(app_date[att_status == 1], na.rm = TRUE),
         first_att = case_when(
           att_status == 1 & app_date == first_attended_date ~ "first_att",
           TRUE ~ "follow_up")) |>
  filter(!!sym(app_date_o) >= first_attended_date) |>
  mutate(count = n()) |>
  filter(count >= 2) |>
  slice_head(n = 2) |> #keep first two appts
  mutate(first_contact = min(app_date),
         days_between_appts = as.numeric(difftime(!!sym(app_date_o), first_contact, units = 'days'))) |> #calculate time between appts
  slice(2) |> #keep second appt only
  filter(quarter_ending %in% date_range) |> #only keep second appts within publication period
  ungroup()  

df_hb_off_avg <- df_treat_wait_offered |>
  group_by(dataset_type, hb_name, quarter_ending) |>
  summarise(avg_days = round(mean(days_between_appts), 1))
