

x=df_rtt %>% 
  mutate(
    app_date_eval = case_when(!is.na(app_date) ~ TRUE,
                              TRUE ~ FALSE),
    ref_rec_date_opti_eval = case_when(!is.na(ref_rec_date_opti) ~ TRUE,
                                       TRUE ~ FALSE),
    act_code_sent_date_eval = case_when(!is.na(act_code_sent_date) ~ TRUE,
                                        TRUE ~ FALSE),
    case_closed_date_eval = case_when(!is.na(case_closed_date) ~ TRUE,
                                        TRUE ~ FALSE)
  ) %>% 
  select(rtt_possible,app_date_eval, ref_rec_date_opti_eval, act_code_sent_date_eval, case_closed_date_eval,
         ref_acc, att_status, app_purpose) %>% 
  distinct()