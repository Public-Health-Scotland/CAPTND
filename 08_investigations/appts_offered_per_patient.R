#######################################
#### Count of sessions per pathway ####
#######################################

#Author: Luke Taylor
#Date: 23/02/2026

#Mean appts offered per pathway by quarter of discharge
#Will currently be impacted by submission of discharge records to clear waiting lists/open cases

#Have filtered to keep only patients that were offered two or more appts, and were therefore engaged in the
#treatment. 

#total appts offered per pathway
df_appts_offered <- df |>
  filter(record_type_label == 'Appointment',
         !!sym(case_closed_date_o) %in% date_range) |> #case closed within publication period
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, 
                   app_date_o, att_status_o, app_purpose_o, case_closed_date_o))) |>
  append_quarter_ending(date_col = "case_closed_date") |>
  group_by(!!!syms(data_keys)) |>
  mutate(count = n()) |> #count number of appts offered
  arrange(!!sym(ucpn_o), !!sym(app_date_o)) |>
  slice_head(n = 1) |> 
  filter(count > 2) |> #only count patients with more than 2 appts in mean
  group_by(dataset_type, hb_name, quarter_ending) |>
  summarise(mean_appts = round(mean(count), 1)) |>
  ungroup()
  

#total appts attended per pathway
df_appts_offered <- df |>
  filter(record_type_label == 'Appointment',
         !!sym(case_closed_date_o) %in% date_range,
         !!sym(att_status_o) == 1) |> #case closed within publication period
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, 
                   app_date_o, att_status_o, app_purpose_o, case_closed_date_o))) |>
  append_quarter_ending(date_col = "case_closed_date") |>
  group_by(!!!syms(data_keys)) |>
  mutate(count = n()) |> #count number of appts offered
  arrange(!!sym(ucpn_o), !!sym(app_date_o)) |>
  slice_head(n = 1) |> 
  filter(count > 2) |> #only count patients with more than 2 appts in mean
  group_by(dataset_type, hb_name, quarter_ending) |>
  summarise(mean_appts = round(mean(count), 1)) |>
  ungroup()


