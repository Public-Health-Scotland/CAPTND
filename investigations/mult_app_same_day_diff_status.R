library(arrow)
library(dplyr)
library(readr)
library(lubridate)

df_glob_swift_completed=read_parquet('../../../output/df_glob_swift_completed_2023-08-25.parquet')

apps_df=df_glob_swift_completed %>% 
  mutate(!!submission_date_o := ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
  select(all_of(c(data_keys,vec_app_cols)),submission_date) %>% 
  distinct() %>% 
  filter(!is.na(!!sym(app_date_o)))

apps=c(data_keys,app_date_o)

apps_df_dup=apps_df %>% 
  group_by(across(all_of(apps)),submission_date) %>% 
  summarise(n_apps_same_day=n(), .groups = 'drop') %>% 
  filter(n_apps_same_day>1) 


apps_df_dup_detail=apps_df_dup%>% 
  inner_join(apps_df, by=c(apps))



apps_by_board=apps_df_dup %>% 
  group_by(dataset_type,hb_name,submission_date) %>% 
  summarise(n=n())