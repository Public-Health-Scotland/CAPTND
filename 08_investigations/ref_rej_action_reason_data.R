
#re-referral data frame
re_ref_df_12mth_rej_acc <- df |>
  remove_borders_int_refs() |>
  filter(record_type_label == 'Referral') |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported, 
         ref_rej_date, ref_rej_reason, ref_rej_act) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti) |>
  group_by(dataset_type, hb_name, chi) |>
  mutate(count = n_distinct(ucpn)) |>
  filter(count >= 2) |>
  mutate(last_ref_date = lag(ref_rec_date_opti),
         last_acc_status = lag(ref_acc_last_reported),
         last_ref_rej_reason = lag(ref_rej_reason),
         last_ref_rej_action = lag(ref_rej_act)) |>
  filter(!is.na(last_ref_date),
         last_ref_date >= ref_rec_date_opti %m-% months(12)) |>
  ungroup() |> select(-count) |>
  append_quarter_ending(date_col = "ref_rec_date_opti") 

re_ref_complete <- re_ref_df_12mth_rej_acc |>
  filter(ref_acc_last_reported == 1 & last_acc_status == 2,
         quarter_ending >= month_start) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup() |>
  group_by(dataset_type) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  arrange(dataset_type, hb_name) |>
  write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/referral_rej_acc_data.xlsx")


#referral rejection reason
lookup_rej_reason <- import('../../../data/captnd_codes_lookup.xlsx', which = 'Rej_Reason') |> 
  rename(ref_rej_reason = REJ_REASON,
         ref_rej_reason_desc = Rej_Reason) |> 
  select(1:2)

non_acceptance_reason <- re_ref_df_12mth_rej_acc |>
  filter(ref_acc_last_reported == 1 & last_acc_status == 2,
         quarter_ending >= month_start) |>
  group_by(dataset_type, hb_name, last_ref_rej_reason) |>
  summarise(count = n()) |>
  group_by(dataset_type, last_ref_rej_reason) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(lookup_rej_reason, by = c("last_ref_rej_reason" = "ref_rej_reason")) |>
  right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |>
  arrange(dataset_type, hb_name, desc(count)) |>
  group_by(dataset_type, hb_name) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 1)) |>
  ungroup()


df_scot_tot <- non_acceptance_reason |> 
  filter(hb_name == 'NHS Scotland') |>
  select(dataset_type, ref_rej_reason_desc, nhsscot_count = count, nhsscot_total = total, nhsscot_prop = prop)


ref_rej_reason_pub_data <- non_acceptance_reason |>
  left_join(df_scot_tot, by = c("dataset_type", "ref_rej_reason_desc")) |> 
  group_by(dataset_type, hb_name) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 1),
         rank = row_number(),
         top5 = case_when(rank >5 ~ "All other referral rejection reason",
                          TRUE ~ ref_rej_reason_desc)) |>
  ungroup() |> 
  group_by(dataset_type, hb_name, top5) |> 
  mutate(count = sum(count)) |>
  group_by(dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6) |>
  add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) %>%
  bind_rows(summarise(.,
                      across(count, sum),
                      across(top5, ~"Total"),
                      across(rank, ~ 7),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  select(dataset_type, hb_name, top5, 
         count, rank, total, prop, nhsscot_prop) |>
  write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/re_referral_rej_reason_data.xlsx")


#referral rejection action
lookup_rej_action <- import('../../../data/captnd_codes_lookup.xlsx', which = 'Rej_Action') |> 
  rename(ref_rej_act = REJ_ACTIONS,
         ref_rej_act_desc = Rej_Action) |> 
  select(1:2)

non_acceptance_action <- re_ref_df_12mth_rej_acc |>
  filter(ref_acc_last_reported == 1 & last_acc_status == 2,
         quarter_ending >= month_start) |>
  group_by(dataset_type, hb_name, last_ref_rej_action) |>
  summarise(count = n()) |>
  group_by(dataset_type, last_ref_rej_action) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(lookup_rej_action, by = c("last_ref_rej_action" = "ref_rej_act")) |>
  right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |>
  arrange(dataset_type, hb_name, desc(count)) |>
  group_by(dataset_type, hb_name) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 1)) |>
  ungroup()


df_scot_tot <- non_acceptance_action |> 
  filter(hb_name == 'NHS Scotland') |>
  select(dataset_type, ref_rej_act_desc, nhsscot_count = count, nhsscot_total = total, nhsscot_prop = prop)


ref_rej_action_pub_data <- non_acceptance_action |>
  left_join(df_scot_tot, by = c("dataset_type", "ref_rej_act_desc")) |> 
  group_by(dataset_type, hb_name) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 1),
         rank = row_number(),
         top5 = case_when(rank >5 ~ "All other referral rejection actions",
                          TRUE ~ ref_rej_act_desc)) |>
  ungroup() |> 
  group_by(dataset_type, hb_name, top5) |> 
  mutate(count = sum(count)) |>
  group_by(dataset_type, hb_name) |>
  filter(rank >= 1 & rank <= 6) |>
  add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) %>%
  bind_rows(summarise(.,
                      across(count, sum),
                      across(top5, ~"Total"),
                      across(rank, ~ 7),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  select(dataset_type, hb_name, top5, 
         count, rank, total, prop, nhsscot_prop) |>
  write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/re_referral_rej_action_data.xlsx")

  