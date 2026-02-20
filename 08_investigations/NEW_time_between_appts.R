###################################################
#### Time between first and second appointment ####
###################################################


#timescales need looked at. DoD won't work well because of ongoing data quality work

df_treat_wait <- df |>
  filter(!!sym(case_closed_date_o) %in% date_range,
         record_type_label == 'Appointment',
         !!sym(att_status_o) == 1) |> 
  select(!!!syms(c(dataset_type_o, hb_name_o, ucpn_o, patient_id_o, ref_rec_date_opti_o, 
                   app_date_o, att_status_o, app_purpose_o, case_closed_date_o))) |>
  append_quarter_ending(date_col = "case_closed_date") |>
  group_by(!!!syms(data_keys)) |>
  mutate(count = n()) |>
  filter(count >= 2) |>
  arrange(!!sym(ucpn_o), !!sym(app_date_o)) |>
  slice_head(n = 2) |>
  mutate(first_contact = min(app_date),
         days_between_appts = as.numeric(difftime(!!sym(app_date_o), first_contact, units = 'days'))) |>
  slice(2) |>
  ungroup()


df_hb_avg <- df_treat_wait |>
  group_by(dataset_type, hb_name, quarter_ending) |>
  summarise(avg_days = round(mean(days_between_appts), 1))
