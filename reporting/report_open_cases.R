library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)

df=read_parquet('../../../output/df_glob_swift_completed_rtt_2023-08-25.parquet')

# last_date_on_file <- list.files(path = "../../../output/removed/", pattern ="swift.*\\.csv$", full.names = FALSE) %>% 
#   map_chr(~str_extract(.,'\\d{4}.+\\d{2}.+\\d{2}')) %>% max(.)
# 
# 
# df <- list.files(path = "../../../output/", pattern ="df_glob_completed_rtt.*\\.parquet$", full.names = TRUE) %>% 
#   map(~.[str_detect(.,last_date_on_file)]) %>% 
#   map_df(~read_parquet(.)) 


df_open_details <- df %>% 
  group_by(across(all_of(data_keys))) %>% 
  filter(any(!is.na(!!sym(app_date_o)))&
           all(is.na(!!sym(case_closed_date_o)))) %>%
  mutate(time_since_last_app=today()-max(!!sym(app_date_o), na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(all_of(data_keys),!!ref_rec_date_opti_o,!!app_date_o, !!case_closed_date_o,time_since_last_app,sub_source_eval) 
  
  
df_open = df_open_details %>% 
  select(-!!app_date_o) %>% 
  distinct() %>% 
  mutate(ref_year = extract_fin_year(!!sym(ref_rec_date_opti_o))) %>% 
  select(-!!ref_rec_date_opti_o)


df_open_lanarkshire=df_open %>% filter(hb_name=='NHS Lanarkshire' & dataset_type=='CAMHS')
nrow(df_open_lanarkshire)
View(df_open_lanarkshire)


