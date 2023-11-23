
df_app %>% 
  filter(dataset_type=='CAMHS',
         !!sym(app_month_o)> (most_recent_month_in_data- months(15))) %>% 
  ggplot(aes(y=n_app_month,
             x=n_app_days_month))+
  geom_point(size=0.5)+
  facet_wrap(~hb_name, scales="free")+
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1, colour = 'red')+
  xlab('Number of appointment days')+
  ylab('Number of appointments')+
  ggtitle('Number of appointments vs appointment days for every board - CAMHS')

ggsave(paste0(appointments_dir,'/app_days_vs_app_number_CAMHS.png'),
       width = 25,
       height =15,
       units = 'cm',
       device = 'png',
       dpi=300,
       bg='white'
       )
           

df_app %>% 
  filter(dataset_type=='PT',
         !!sym(app_month_o)> (most_recent_month_in_data- months(15))) %>% 
  ggplot(aes(y=n_app_month,
             x=n_app_days_month))+
  geom_point(size=0.5)+
  facet_wrap(~hb_name, scales="free")+
  theme_minimal()+
  geom_abline(intercept = 0, slope = 1, colour = 'red')+
  xlab('Number of appointment days')+
  ylab('Number of appointments')+
  ggtitle('Number of appointments vs appointment days for every board - PT')

ggsave(paste0(appointments_dir,'/app_days_vs_app_number_PT.png'),
       width = 25,
       height =15,
       units = 'cm',
       device = 'png',
       dpi=300,
       bg='white'
)
