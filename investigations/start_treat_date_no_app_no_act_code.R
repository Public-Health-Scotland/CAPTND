#JBS
#22/01/2024

#Id pat with no act code date an no app date that are still classified as cases open

df_open <- df %>% 
  group_by(across(all_of(data_keys))) %>% 
  filter(!!sym(rtt_eval_o) %in% c("seen - active",
                                  "seen - online - active")) %>% 
  mutate(weeks_since_last_app= case_when(
    any(!is.na(!!sym(app_date_o))) ~ as.numeric(ceiling(difftime(
      most_recent_month_in_data, max(!!sym(app_date_o), na.rm = TRUE), units = "weeks"))),
    TRUE ~ NA),
    weeks_since_code_sent= case_when(
      any(!is.na(!!sym(act_code_sent_date_o))) ~ as.numeric(ceiling(difftime(
        most_recent_month_in_data, max(!!sym(act_code_sent_date_o), na.rm = TRUE), units = "weeks"))),
      TRUE ~ NA),
    max_app_date = max(!!sym(app_date_o), na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(all_of(data_keys),!!ref_rec_date_opti_o, max_app_date, !!act_code_sent_date_o,
         weeks_since_last_app, weeks_since_code_sent, sub_source_eval, !!rtt_eval_o) %>% 
  distinct()

neg=df_open %>% filter(weeks_since_code_sent<0)

df_open_w=df_open %>% filter(is.na(weeks_since_last_app) & is.na(weeks_since_code_sent)) %>% 
  select(all_of(data_keys),!!ref_rec_date_opti_o, max_app_date, !!act_code_sent_date_o,
         weeks_since_last_app, weeks_since_code_sent, sub_source_eval, !!rtt_eval_o) %>% 
  distinct()

df_issues=df_open_w %>% select(all_of(data_keys)) %>% distinct() %>% inner_join(df)


#they have no app but have a start treat date

#will quantify


z=df_issues %>% select(all_of(data_keys), header_date, treat_start_date) %>% 
  group_by(across(all_of(data_keys))) %>% 
  mutate(header_date=max(header_date)) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(header_date, hb_name, dataset_type) %>% 
  summarise(n=n(), .groups = 'drop')
  
