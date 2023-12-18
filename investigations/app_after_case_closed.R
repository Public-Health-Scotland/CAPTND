library(conflicted)
conflict_prefer('select','dplyr')
conflict_prefer('mutate','dplyr')
conflict_prefer('filter','dplyr')


#read most recent RTT eval file
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))


#find app date later than case closed dates

df_app <- df %>% 
  select(all_of(c(data_keys, case_closed_date_o, app_date_o))) %>% 
  group_by(across(all_of(data_keys))) %>% 
  mutate(max_app_date = max(!!sym(app_date_o), na.rm = TRUE),
         !!case_closed_date_o := !!sym(case_closed_date_o)) %>% 
  select(-!!app_date_o) %>% 
  filter(!is.na(max_app_date) & !is.na(!!sym(case_closed_date_o))) %>% 
  ungroup() %>% 
  distinct()

df_app_after_discharge <- df_app %>% 
  filter(max_app_date > !!sym(case_closed_date_o)) %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
  summarise(n=n())

write_csv_arrow(df_app_after_discharge, paste0(appointments_dir,'/apps_after_discharge.csv'))