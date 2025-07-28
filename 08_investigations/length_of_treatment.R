
# Author: Luke Taylor
# Date: 2025-07-15

# Count the avg number of appts per patient pathway and the average number of days between referral date and
# either latest app date or discharge date

##Average length of treatment in days
#identify treatment end date
df_length <- df |>
  remove_borders_int_refs() |>
  arrange(ucpn, app_date) |>
  select(!!!syms(data_keys), app_month, app_date, ref_rec_date_opti, app_purpose, att_status,
         case_closed_date) |>
  group_by(!!!syms(data_keys)) |>
  mutate(has_treat_end_date = fcase(any(is.na(app_date) & is.na(case_closed_date)), FALSE,
                                         default = TRUE)) |>
  filter(has_treat_end_date == TRUE) |>
  mutate(latest_app_date = ifelse(max(app_date, na.rm = TRUE) == -Inf, NA, max(app_date, na.rm = TRUE)),
         latest_app_date = as.Date(latest_app_date)) |>
  mutate(last_contact = case_when(!is.na(case_closed_date) & !is.na(latest_app_date) & case_closed_date >= latest_app_date ~ case_closed_date,
                               !is.na(case_closed_date) & !is.na(latest_app_date) & case_closed_date < latest_app_date ~ latest_app_date,
                               is.na(case_closed_date) & !is.na(latest_app_date) ~ latest_app_date,
                               !is.na(case_closed_date) & is.na(latest_app_date) ~ case_closed_date,
                               TRUE ~ NA_Date_)) |>
  ungroup()

#count average length of treatment in days
df_treat_length <- df_length |>
  mutate(ref_year = lubridate::year(!!sym(ref_rec_date_opti_o)),
         treat_length = as.numeric(difftime(last_contact, ref_rec_date_opti, units = 'days'))) |>
  select(ucpn, hb_name, dataset_type, ref_year, treat_length) |>
  distinct() |>
  group_by(ref_year, hb_name, dataset_type) |>
  summarise(avg_treat_length = round(mean(treat_length), 2)) |>
  arrange(hb_name, ref_year)
  


##Average number of appointments for patients with a case closed date
#identify treatment end date
df_app_count <- df |>
  remove_borders_int_refs() |>
  arrange(ucpn, app_date) |>
  filter(is_case_closed == TRUE | has_any_app_date == TRUE,
         !is.na(app_date)) |>
  select(!!!syms(data_keys), app_date, ref_rec_date_opti) |>
  mutate(ref_year = lubridate::year(ref_rec_date_opti)) |>
  group_by(!!!syms(data_keys)) |>
  mutate(count = n()) |>
  select(-app_date) |>
  distinct() |>
  group_by(hb_name, dataset_type) |>
  summarise(avg_apps = round(mean(count),2)) |>
  ungroup()





