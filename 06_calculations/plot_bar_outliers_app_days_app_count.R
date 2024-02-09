####################################################.
### Plot appointment days and appointment number ###.
####################################################.


#Plot app days and numbers 
#author: JBS
#date: 08/02/24


plot_bar_outliers_app_days_app_count <- function(df_app_pre_calc, ds_type){
  
  p2 <- df_app_pre_calc %>% 
    mutate(n_same_day_app = case_when(n_app_patient_same_day == 1 ~ '1',
                                 n_app_patient_same_day <= 3 ~ '2-3',
                                 TRUE ~ '4+')) %>% 
    group_by(across(all_of(c(dataset_type_o, hb_name_o, app_month_o))),n_same_day_app) %>% 
    summarise(n=n(), .groups = 'drop') %>% 
    group_by(across(all_of(c(dataset_type_o, app_month_o))),n_same_day_app) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) %>% 
    filter(!!sym(dataset_type_o)==ds_type,
           !!sym(app_month_o)> (most_recent_month_in_data  %m-% months(15))) %>%
    ggplot( aes(x=app_month, 
                y=n, 
                group=n_same_day_app, 
                fill=n_same_day_app,
                text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Appointment month: ", gsub('\n','-',app_month), "<br>",
                    "Number of apps/day: ", n_same_day_app, "<br>",
                    "Number of cases: ",n
                  ))) +
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
      theme_minimal()+
      scale_fill_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("Number")+
      xlab("Appointment month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%y")+
      labs(title=paste0("Appointments per day by month - ",
                        ds_type),
           colour= "Apps per day")+
      theme(plot.title = element_text(hjust = 0.5, size = 25))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales="free_y")+
      theme(panel.spacing.x= unit(-1.5, "lines"),
            panel.spacing.y = unit(0, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=12, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    
    fig2=ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(appointments_dir,
                    '/plot_bar_outliers_app_days_app_count_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    
    
    
}

