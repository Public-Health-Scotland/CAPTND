



df_seen = read_csv_arrow(paste0(patients_seen_dir,'/patients_waitingTimes_seen_subSource.csv'))


db_date = readRDS('../../../../../../MentalHealth3/CAMHS_PT_dashboard/submissionProcessing/data/CAMHS_DB.rds') %>% 
  bind_rows(readRDS('../../../../../../MentalHealth3/CAMHS_PT_dashboard/submissionProcessing/data/PsychologicalTherapies_DB.rds')) %>% 
  filter(adj_unadj=='u') %>%
  select(hb_name=HB,
         dataset_type=dataset,
         app_month=date,
         waiting_time=waitingTimeWeeks,
         n_aggregate=nPatients) %>% 
  distinct() %>% 
  filter(app_month >= max(!!sym(app_month_o)) - months(15)) %>% 
  mutate(hb_name=case_when(str_detect(hb_name,'Ayrshire') ~ 'NHS Ayrshire and Arran',
                           str_detect(hb_name,'Borders') ~ 'NHS Borders',
                           str_detect(hb_name,'Dumfries') ~ 'NHS Dumfries and Galloway',
                           str_detect(hb_name,'Fife') ~ 'NHS Fife',
                           str_detect(hb_name,'Grampian') ~ 'NHS Grampian',
                           str_detect(hb_name,'Glasgow') ~ 'NHS Greater Glasgow and Clyde',
                           str_detect(hb_name,'Highland') ~ 'NHS Highland',
                           str_detect(hb_name,'Lanarkshire') ~ 'NHS Lanarkshire',
                           str_detect(hb_name,'Lothian') ~ 'NHS Lothian',
                           str_detect(hb_name,'Shetland') ~ 'NHS Shetland',
                           str_detect(hb_name,'Western') ~ 'NHS Western Isles',
                           str_detect(hb_name,'Orkney') ~ 'NHS Orkney')) %>% 
  full_join(df_seen, by = join_by(!!app_month_o, !!hb_name_o, !!dataset_type_o, waiting_time)) %>% 
  mutate(n_aggregate=replace_na(n_aggregate, 0),
         n=replace_na(n, 0),
         captnd_perc_agg=round(n*100/n_aggregate,2))
  


