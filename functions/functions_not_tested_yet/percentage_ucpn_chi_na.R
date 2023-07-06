

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
  mutate(dateSub=ym(format(dateSub, "%Y-%m"))) %>% 
  ggplot( aes(x=dateSub, y=ucpn_or_chi_na_perc, group=dataset_type, colour=dataset_type)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(
    #labels = add_fisc_year_lab,
    minor_breaks = NULL,
    breaks = seq.Date(
      from = min(df_stats$dateSub),
      to = max(df_stats$dateSub),
      by = "month"))+
  facet_wrap(~ hb_name)

ggsave("../../../output/percentagePlots.png",
       width = 20,
       height = 16,
       units = c("cm"),
       dpi = 300,
       bg='white')
# gramp=df_stats %>% filter(hb_name=='NHS Grampian')
# 
# 
# gramp %>% filter(dateSub>(max(dateSub)- years(1))) %>% 
#   ggplot( aes(x=dateSub, y=ucpn_or_chi_na_perc, group=dataset_type, colour=dataset_type)) +
#   geom_line()
# ggsave("../../../output/percentagePlots_Gramp.png",
#        width = 15,
#        height = 12,
#        units = c("cm"),
#        dpi = 300)