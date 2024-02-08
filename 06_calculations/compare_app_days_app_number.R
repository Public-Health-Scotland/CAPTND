#######################################################.
### Compare appointment days and appointment number ###.
#######################################################.


#function to compare app days and numbers 
#author: JBS
#date: 04/01/24


compare_app_days_app_number <- function(df, ds_type){
  df %>% 
    filter(dataset_type==ds_type,
           !!sym(app_month_o)> (most_recent_month_in_data  %m-% months(15))) %>% 
    ggplot(aes(y=n_app_month,
               x=n_app_days_month))+
    geom_point(size=0.5)+
    facet_wrap(~hb_name, scales="free")+
    theme_minimal()+
    geom_abline(slope = seq(0.9, 1.1, 0.001), color = "grey80", intercept = 0, alpha = 0.05)+
    geom_abline(intercept = 0, slope = 1, colour = 'red')+
    xlab('Number of appointment days')+
    ylab('Number of appointments')+
    ggtitle(paste0('Number of appointments vs appointment days for every board - ', ds_type))
  
  ggsave(paste0(appointments_dir,'/app_days_vs_app_number_', ds_type,'.png'),
         width = 25,
         height =15,
         units = 'cm',
         device = 'png',
         dpi=300,
         bg='white'
  )
}

