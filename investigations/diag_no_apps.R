

na_rate <- df_glob_swift %>% 
  select(all_of(ref_cols)) %>%  
  summarise_all(funs(sum(is.na(.)))) %>% 
  mutate(type='before') %>% 
  select(type,everything())



na_rate_after_fun <- df_glob_swift_refs %>% 
  ungroup() %>% 
  select(all_of(ref_cols)) %>%  
  summarise_all(funs(sum(is.na(.))))%>% 
  mutate(type='after') %>% 
  select(type,everything())

na_rate_comp=bind_rows(na_rate,na_rate_after_fun)

rm(na_rate,na_rate_after_fun)


diag=df_glob_swift_refs %>% filter(!is.na(diag_1)) %>% 
  select(all_of(data_keys)) %>% 
  distinct() %>% 
  head(1) 

p1=df_glob_swift_refs %>% inner_join(diag,by=data_keys)


n_recs_sameFileInfo1=df_glob_swift_refs %>% group_by(patient_id,ucpn,dataset_type,hb_name,file_id, app_date, diag_1) %>% 
  #select(app_date, diag_1) %>% 
  summarise(n=n())%>% 
  filter(!(is.na(app_date)&is.na(diag_1)))

n_recs_sameFileInfo2=df_glob_swift_refs %>% group_by(patient_id,ucpn,dataset_type,hb_name,app_date, diag_1) %>% 
  #select(app_date, diag_1) %>% 
  summarise(n=n()) %>% 
  filter(!(is.na(app_date)&is.na(diag_1)))


pathWithDiag=df_glob_swift_refs %>% filter(!is.na(diag_1)&sub_source=='swift') %>% select(patient_id,ucpn,dataset_type,hb_name,file_id)

pat_investig=df_glob_swift_refs %>% 
  inner_join(pathWithDiag, by=c('patient_id','ucpn','dataset_type','hb_name','file_id')) %>% 
  group_by(patient_id,ucpn,dataset_type,hb_name,file_id) %>% 
  summarise(n=n()) 

patWithApps=pat_investig %>% filter(n>1) %>% pull(patient_id)

pats_no_apps=pat_investig %>% filter(!patient_id %in% patWithApps)


totalPathways=df_glob_swift_refs %>% 
  select(all_of(data_keys)) %>% 
  distinct() %>% 
  group_by(dataset_type,hb_name) %>% 
  summarise(total=n())  

pathWithDiagNoApps=pats_no_apps %>% 
  select(all_of(data_keys)) %>% 
  distinct() %>% 
  group_by(dataset_type,hb_name) %>% 
  summarise(n=n()) 


noApps_stat=full_join(totalPathways,pathWithDiagNoApps) %>% 
  mutate(n=replace_na(n,0),
         perc=n*100/total)
  
