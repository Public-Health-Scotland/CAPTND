
#publication period
month_end <- as.Date("2026-09-01")
month_start <- as.Date(month_end - months(14))

#follow up period
follow_up_end <- as.Date(ceiling_date(month_end, "month") - days(1) - months(12)) 
follow_up_start <- as.Date(month_start - months(12))

#denominator
#all rejected referrals eligible for 12 month follow-up
rejected_refs <- df |>
  remove_borders_int_refs() |>
  filter(record_type_label == 'Referral',
         ref_rec_date_opti >= follow_up_start & ref_rec_date_opti <= follow_up_end,
         ref_acc_last_reported == 2) |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported) |>
  distinct() |>
  group_by(dataset_type, hb_name) |>
  summarise(total = n())

#everyone referred between Jul-24 (12mth before pub period) and Sep-25 (12mth before end of pub period)

#numerator
#referrals which go on to be accepted within 12 months of non-accepted referral
rej_refs_accept <- df |>
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
         last_ref_date >= follow_up_start,
         ref_acc_last_reported == 1 & last_acc_status == 2) |>
  ungroup() |> select(-count) |>
  group_by(dataset_type, hb_name) |>
  summarise(count = n()) |> ungroup()


re_ref_rate <- rejected_refs |>
  left_join(rej_refs_accept, by = c("dataset_type", "hb_name")) |>
  mutate(prop = round(count/total*100, 1))

#how many patients within that window have more than one referral 

df_check <- df |>
  filter(record_type_label == 'Referral',
         ref_rec_date_opti >= follow_up_start & ref_rec_date_opti <= month_end) |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported) |>
  distinct() |>
  group_by(dataset_type, hb_name, chi) |>
  mutate(count = n()) |>
  filter(count >= 2) |>
  arrange(dataset_type, chi)






