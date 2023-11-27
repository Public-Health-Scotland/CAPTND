unknown = df_rtt2 %>% filter(rtt_possible == 'rtt_not possible - unknown') %>% 
  select(rtt_possible,
         ref_acc_last_reported, 
         has_any_app_date, 
         app_purpose,
         att_status_o,
         has_act_code_sent_date,
         is_case_closed         ) %>% 
  distinct()
  
u1 = df_rtt2 %>% filter(rtt_possible == 'rtt_not possible - unknown',
                        ref_acc_last_reported==1) %>% 
  group_by(!!!syms(data_keys)) %>% 
  
  mutate(x=case_when( has_any_app_date == TRUE &
                        has_ref_rec_date_opti == TRUE &
                        is_case_closed == TRUE &
                        ref_acc_last_reported == 1 &
                        any(
                          !is.na(!!sym(app_date_o)) &
                            !!sym(att_status_o) == 1 &
                            !!sym(app_purpose_o) %in% c(1,4,6)
                          
                        ) &
                        !any(
                          !is.na(!!sym(app_date_o)) &
                            !!sym(app_purpose_o) %in% c(2,3,5) 
                        ) ~ 'case closed after assessment',
                      TRUE ~ 'no'),
         .after=rtt_possible)
                        

u1_details = df_rtt2 %>% inner_join(u1)

unknown_accepted= unknown %>% filter(ref_acc_last_reported==1)

ccaa=df_rtt2 %>% filter(rtt_possible=='case closed after assessment') %>% 
  bind_rows(u1) %>% 
  select(rtt_possible,
                      ref_acc_last_reported, 
                      has_any_app_date, 
                      app_purpose,
                      att_status_o,
                      has_act_code_sent_date,
                      is_case_closed         ) %>% 
  distinct()




similar_but_diff = df_rtt2 %>% 
  filter(rtt_possible %in% c('case closed after assessment','rtt_not possible - unknown'),
         has_any_app_date == TRUE, 
         app_purpose==1,
         att_status==1,
         ref_acc_last_reported==1,
         has_act_code_sent_date==FALSE,
         is_case_closed==TRUE   ) %>% 
  select(all_of(data_keys),all_of(vec_rtt_adj_cols), rtt_possible,ref_acc_last_reported,has_any_app_date,is_case_closed) %>% 
  distinct()


