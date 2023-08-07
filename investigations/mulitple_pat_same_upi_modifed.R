#Addd file_id and ref_rec-date to list of chis as requested by HBs

upi_and_chi_modified=df_glob_swift_refs %>% select(hb_name, dataset_type,file_id, 
                                                   ref_rec_date,upi,chi)


#Making doc for Dougie with UPIs with mult chis, including chi, file_id and ref_rec-date
chis_per_upi_modified = chis_per_upi %>% 
  group_by(hb_name,dataset_type) %>% 
  inner_join(upi_and_chi_modified, by=c('upi','hb_name', 'dataset_type'))%>% 
  ungroup()

write_csv(chis_per_upi_modified, '../../../output/investigations/multi_chis_per_upi_modified.csv')

chis_per_upi_modified_list=chis_per_upi_modified %>% 
  group_by(hb_name) %>% 
  group_split()

for(df in chis_per_upi_modified_list){
  df=df %>% distinct()
  fname=unique(df$hb_name)
  write_csv(df,paste0('../../../output/investigations/multiple_chi_per_upi/mult_chi_per_upi_modified',fname,'.csv'))
}
