
#Rereferral query

#1.Number of children and young people referred to CAMHS more than once within a 12-month period.
#2.Number of repeat referrals following an earlier referral that was not accepted.
#3.Number of repeat referrals subsequently accepted for assessment or intervention.
#4.Where recorded, number of children and young people re-referred following previous 
#involvement with third-sector or community-based mental health services.

#Query 1
re_ref_df_12mth <- df |>
  filter(dataset_type == 'CAMHS') |>
  select(dataset_type, hb_name, ucpn, chi, ref_rec_date_opti, ref_acc_last_reported) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti) |>
  group_by(hb_name, chi) |>
  mutate(count = n_distinct(ucpn)) |>
  filter(count >= 2) |>
  
  mutate(last_ref_date = lag(ref_rec_date_opti)) |>
  filter(!is.na(last_ref_date),
         last_ref_date >= ref_rec_date_opti %m-% months(12)) |>
  ungroup() |>
  mutate(fin_year = phsmethods::extract_fin_year(ref_rec_date_opti)) |>
  group_by(fin_year, hb_name) |>
  summarise(n_patients = n_distinct(chi))
  

