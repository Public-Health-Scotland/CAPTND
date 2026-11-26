
#publication period
month_end <- as.Date("2026-09-01")
month_start <- as.Date(month_end - months(14))

#follow up period
follow_up_end <- as.Date(ceiling_date(month_end, "month") - days(1) - months(12)) 
follow_up_start <- as.Date(month_start - months(12))


# all patient re-referrals within 12 months in dataset 
all_re_referrals <- df |>
  remove_borders_int_refs() |>
  filter(record_type_label == 'Referral') |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported, 
         ref_rej_date, ref_rej_reason, ref_rej_act) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti) |>
  group_by(dataset_type, hb_name, chi) |>
  mutate(count = n_distinct(ucpn)) |> ungroup() |>
  filter(count >= 2)

#re-referrals within 12 months that have taken place during 15 month publication period 
all_re_ref_pub_period <- all_re_referrals|>
  mutate(last_ref_date = lag(ref_rec_date_opti),
         last_acc_status = lag(ref_acc_last_reported),
         last_ref_rej_reason = lag(ref_rej_reason),
         last_ref_rej_action = lag(ref_rej_act)) |>
  filter(ref_rec_date_opti >= month_start,
         last_ref_date >= follow_up_start,
         last_ref_date >= ref_rec_date_opti %m-% months(12),
         !is.na(last_ref_date)) 

#all re-referrals during 15 month publication period - DENOMINATOR
re_ref_denominator <- all_re_ref_pub_period |>
  group_by(dataset_type, hb_name) |>
  summarise(total = n()) |> ungroup()

#all accepted re-referrals following a non-accepted referral during 15 month publication period - NUMERATOR
re_ref_numerator <- all_re_ref_pub_period |>
  filter(ref_acc_last_reported == 1 & last_acc_status == 2) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup()

#proportion of re-referrals that were accepted following previous non-acceptance
re_ref_rate <- re_ref_denominator |>
  left_join(re_ref_numerator, by = c("dataset_type", "hb_name")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  group_by(dataset_type) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  mutate(prop = round(count/total*100, 1)) |>
  write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/re_referral_rate_data.xlsx")



