

x=df_rtt %>% 
  group_by(!!!syms(data_keys)) %>% 
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
  ungroup() %>% 
  select(rtt_possible,app_date_eval, ref_rec_date_opti_eval, act_code_sent_date_eval, case_closed_date_eval,
         ref_acc, att_status, app_purpose) %>% 
  distinct()



y=df_rtt %>% 
  group_by(!!!syms(data_keys)) %>% 
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
  select(ucpn, patient_id, hb_name, dataset_type,rtt_possible,app_date_eval, ref_rec_date_opti_eval, act_code_sent_date_eval, case_closed_date_eval,
         ref_acc, att_status, app_purpose) %>% 
  distinct()

y_closed_after_assessment = y %>% filter(rtt_possible == 'case closed after assessment')


z=df_rtt2 %>% 
  select(rtt_possible,
         has_any_app_date,
         has_act_code_sent_date,
         is_case_closed,
         ref_acc_last_reported, att_status, app_purpose) %>% 
  distinct()


z1=df_rtt2 %>% 
  filter(rtt_possible=='unknown') %>% 
  group_by(!!!syms(data_keys)) %>% 
  filter(any(app_purpose %in% c(2,3,5) & att_status== 1)) %>% 
  select(header_date, everything())
  
  
