library(stringr)
library(ggplot2)
library(plotly)

df_glob_swift_completed_rtt=read_parquet('../../../output/df_glob_swift_completed_rtt_2023-09-21.parquet')

df_referrals= read_csv_parquet('../../../output/calculations/referrals.csv')

aggregate_CAMHS=read_csv_arrow('../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/PatientsSeen_CAMHS_2023-06-01.csv') %>% 
  filter(variables_mmi %in% c('All Referrals Received','Referrals Accepted')) %>% 
  mutate(!!dataset_type_o := 'CAMHS',
         !!ref_acc_o := case_when(variables_mmi=='All Referrals Received' ~ 'total',
                                  variables_mmi=='Referrals Accepted' ~ 'accepted')) %>% 
  pivot_longer(starts_with('2'), names_to = 'referral_month', values_to = 'n_aggregate')


aggregate_PT=read_csv_arrow('../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/PatientsSeen_PT_2023-06-01.csv') %>% 
filter(variables_mmi %in% c('ReferralsReceived','ReferralsAccepted')) %>% 
  mutate(!!dataset_type_o := 'PT',
         !!ref_acc_o := case_when(variables_mmi=='ReferralsReceived' ~ 'total',
                                  variables_mmi=='ReferralsAccepted' ~ 'accepted')) %>% 
  pivot_longer(starts_with('2'), names_to = 'referral_month', values_to = 'n_aggregate')


aggregate=bind_rows(aggregate_CAMHS,aggregate_PT) %>% 
  select(-variables_mmi) %>% 
  rename(!!hb_name_o := HB_new) %>% 
  mutate(referral_month = as.Date(referral_month))


all_refs = df_referrals %>% 
  filter(!!sym(ref_acc_o) %in% c('total', 'accepted'),
         referral_month %in% aggregate$referral_month) %>% 
  inner_join(aggregate,by = join_by('referral_month', !!hb_name_o, !!dataset_type_o, !!ref_acc_o)) %>% 
  mutate(captnd_perc_agg=n*100/n_aggregate)



p1 <- all_refs %>% 
  filter(!!sym(dataset_type_o)=='CAMHS') %>% 
  ggplot( aes(x=referral_month, 
              y=captnd_perc_agg, 
              group=ref_acc, 
              colour=ref_acc,
              text = paste0(
                "Health Board: ", hb_name, "<br>",
                "Referral month: ", gsub('\n','-',referral_month), "<br>",
                "Referral measure: ", gsub('_',' ',ref_acc), "<br>",
                "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                "n CAPTND: ",n, " | n aggregate: ", n_aggregate
              ))) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_hline(yintercept=100, linetype='dashed', color="#83BB26")+
  scale_colour_manual(values=c("#3F3685",
                               "#9B4393"))+
  ylab("% similarity with aggregate")+
  xlab("Referral month")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # scale_x_date(
  #   minor_breaks = NULL,
  #   breaks = seq.Date(
  #     from = min(df1$submission_date, na.rm = T),
  #     to = max(df1$submission_date, na.rm = T),
  #     by = "month"))+
  labs(title=paste0("Referrals - CAPTND comparison to aggregate (100%) - CAMHS"),
       colour= "referrals measure")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(hb_name, levels=c(level_order)))+
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(1, "lines"))


fig1=ggplotly(p1, tooltip = "text") 



p2 <- all_refs %>% 
  filter(!!sym(dataset_type_o)=='PT') %>% 
  ggplot( aes(x=referral_month, 
              y=captnd_perc_agg, 
              group=ref_acc, 
              colour=ref_acc,
              text = paste0(
                "Health Board: ", hb_name, "<br>",
                "Referral month: ", gsub('\n','-',referral_month), "<br>",
                "Referral measure: ", gsub('_',' ',ref_acc), "<br>",
                "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                "n CAPTND: ",n, " | n aggregate: ", n_aggregate
              ))) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_hline(yintercept=100, linetype='dashed', color="#83BB26")+
  scale_colour_manual(values=c("#3F3685",
                               "#9B4393",
                               "#0078D4",
                               "#83BB26"))+
  ylab("% similarity with aggregate")+
  xlab("Referral month")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  # scale_x_date(
  #   minor_breaks = NULL,
  #   breaks = seq.Date(
  #     from = min(df1$submission_date, na.rm = T),
  #     to = max(df1$submission_date, na.rm = T),
  #     by = "month"))+
  labs(title=paste0("Referrals - CAPTND comparison to aggregate (100%) - PT"),
       colour= "referrals measure")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(hb_name, levels=c(level_order)))+
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(1, "lines"))


fig2=ggplotly(p2, tooltip = "text") 

htmlwidgets::saveWidget(
  widget = fig1, #the plotly object
  file = paste0('../../../output/calculations/referrals/',
                'plot_referrals_CAMHS',
                ".html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)


htmlwidgets::saveWidget(
  widget = fig2, #the plotly object
  file = paste0('../../../output/calculations/referrals/',
                'plot_referrals_PT',
                ".html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)


