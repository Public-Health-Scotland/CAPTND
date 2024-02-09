

aa_data = df %>% 
  filter(rtt_eval %in% c('rtt not possible - patient had appt and ref is pending',
                         'rtt not possible - app with no referral acc') &
           str_detect(hb_name, regex('Ayrshire', ignore_case = TRUE))) %>% 
  select(all_of(data_keys)) %>% 
  distinct()


  write_csv_arrow(aa_data, paste0(data_export_dir,'/A&A_appointments_no_ref_accepted.csv'))
  