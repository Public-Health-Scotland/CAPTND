

df_first_contact <- df %>%
  remove_borders_int_refs() %>%
  filter(!!sym(att_status_o) == 1 &
           !is.na(app_date_o)) %>%
  arrange(ucpn, app_date) %>%
  group_by(!!!syms(data_keys)) %>%
  slice_head() %>%
  ungroup() %>%
  select(!!!syms(data_keys), app_date) %>%
  mutate(flag = 1) %>%
  ungroup()



df_first_contact_dna <- df |>
  filter(!is.na(app_date)) |>
  arrange(ucpn, app_date) |>
  select(!!!syms(data_keys), app_month, app_date, app_purpose, att_status) |>
  left_join(df_first_contact, by = c('dataset_type', 'hb_name', 'ucpn', 'patient_id', 'app_date')) |>
  group_by(!!!syms(data_keys)) |>
  fill("flag", .direction = "down") |>
  filter(is.na(flag) & att_status == 8) |>
  ungroup() |>
  group_by(app_month, hb_name, dataset_type) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  filter(app_month %in% month_range) %>%
  group_by(app_month, !!sym(dataset_type_o)) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop"))


