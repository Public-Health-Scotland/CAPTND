#complete appoitments that are not NA for app_date with referral info


ref_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o, 
ref_rej_date_o, ref_rej_reason_o, rej_rej_act_o, act_code_sent_date_o)


appt_na <- df %>% filter(is.na(!!sym(app_date_o)))

appt_something <- df %>% filter(!is.na(!!sym(app_date_o)))

df_completed <- appt_something %>% 
  group_by(!!sym(data_keys)) %>% 
  fill(ref_cols) %>% 
  bind_rows(appt_na)