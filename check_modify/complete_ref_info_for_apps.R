#complete appoitments that are not NA for app_date with referral info
#not working as we wanted

ref_cols <- c(ref_date_o, ref_rec_date_o, ref_source_o, ref_reason_o, ref_acc_o, 
ref_rej_date_o, ref_rej_reason_o, ref_rej_act_o, act_code_sent_date_o)


appt_na <- df %>% filter(is.na(!!sym(app_date_o))) 

appt_something <- df %>% filter(!is.na(!!sym(app_date_o))) 

df_completed <- appt_something %>% 
  group_by_at(data_keys) %>% 
  fill(ref_cols) %>% 
  bind_rows(appt_na)

#work on group by language to fit recent dplyr syntax

df_pre=data.frame(x=df$ref_rec_date, label='pre')
  
df_post=data.frame(x=df_completed$ref_rec_date, label='post')

df_test=bind_rows(df_pre,df_post) %>% 
  mutate(eval=case_when(is.na(x)~0,
                        TRUE~ 1)) %>% 
  group_by(label) %>% 
  summarise(t=sum(eval))











