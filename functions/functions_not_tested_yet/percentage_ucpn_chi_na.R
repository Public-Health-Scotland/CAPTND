

df_stats <- df_swift_raw %>%
  null_to_na() %>% 
  correct_hb_names() %>% 
  mutate(dateSub=ym(format(!!sym(header_date_o), "%Y-%m"))) %>% 
  select(!!hb_name_o,!!dataset_type_o,!!ucpn_o,!!chi_o,dateSub) %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o),dateSub) %>% 
  summarise(totalRows=n(),
            chi_na=sum(is.na(!!sym(chi_o))),
            ucpn_na=sum(is.na(!!sym(ucpn_o))),
            ucpn_and_chi_na=sum(is.na(!!sym(ucpn_o)) & is.na(!!sym(chi_o))),
            ucpn_or_chi_na=sum(is.na(!!sym(ucpn_o)) | is.na(!!sym(chi_o)))) %>% 
  ungroup() %>% 
  group_by(dateSub, !!sym(dataset_type_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"NHS Scotland"))) %>% 
  group_by(!!sym(hb_name_o)) %>%
  mutate(chi_na_perc= round((chi_na * 100)/totalRows, 3),
            ucpn_na_perc= round((ucpn_na * 100)/totalRows, 3),
            ucpn_and_chi_na_perc= round((ucpn_and_chi_na * 100)/totalRows, 3),
            ucpn_or_chi_na_perc= round((ucpn_or_chi_na * 100)/totalRows, 3),
         .after=dateSub) %>% 
  ungroup()



df_stats %>% filter(dateSub>(max(dateSub)- years(1))) %>% 
  ggplot( aes(x=dateSub, y=ucpn_or_chi_na_perc, group=1, colour=dataset_type)) +
  geom_line()+
  facet_wrap(~ hb_name)

