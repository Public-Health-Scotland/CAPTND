#complete appointments that are not NA for app_date with referral info
#not working as we wanted

ref_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o, 
ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)



df_completed <- df_glob_swift %>% 
  group_by_at(data_keys) %>% 
  arrange(patient_id, ucpn,dataset_type,hb_name,ref_rec_date) %>% 
  fill(ref_cols) %>% 
  select(chi, ucpn, ref_rec_date,app_date,`patient_id`, `dataset_type`, `hb_name`,sub_source) %>% 
  arrange(patient_id,ucpn)






# appt_na <- df_completed %>% filter(is.na(!!sym(app_date_o))) 
# 
# appt_something <- df_completed %>% filter(!is.na(!!sym(app_date_o))) 
# appt_something2 = appt_something %>% filter(chi %in% c('0609565192','2301080846')) %>% 
#   select(chi, ucpn, ref_rec_date,app_date)
# apptAll2=df %>% filter(chi %in% c('0609565192','2301080846')) %>% 
#   select(chi, ucpn, ref_rec_date,app_date,`patient_id`, `dataset_type`, `hb_name`)




test <- df_completed %>% # see if filling in is working
  select(sym(patient_id_o), sym(chi_o), sym(ucpn_o), sym(dataset_type_o), sym(hb_name_o),
         sym(ref_rec_date_o), sym(app_date_o) ) %>%
  sort(sym(patient_id_o))

#work on group by language to fit recent dplyr syntax

df_pre=data.frame(x=df$ref_rec_date, label='pre')
  
df_post=data.frame(x=df_completed$ref_rec_date, label='post')

df_test=bind_rows(df_pre,df_post) %>% 
  mutate(eval=case_when(is.na(x)~0,
                        TRUE~ 1)) %>% 
  group_by(label) %>% 
  summarise(t=sum(eval))











