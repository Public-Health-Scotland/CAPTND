####################################################.
### Plot appointment days and appointment number ###.
####################################################.


#Plot app days and numbers 
#author: JBS
#date: 08/02/24


plot_rate_app_days_app_count <- function(df_app){
  
  p2 <- df_app %>% 
    mutate(rate = n_app_month/n_app_days_month) %>% 
    filter(!!sym(app_month_o)> (most_recent_month_in_data  %m-% months(15))) %>%
    ggplot( aes(x=app_month, 
                y=rate, 
                group=dataset_type, 
                colour=dataset_type,
                text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Appointment month: ", gsub('\n','-',app_month), "<br>",
                    "Dataset type: ", dataset_type, "<br>",
                    "Rate count/days: ",rate
                  ))) +
      geom_line()+
      geom_point()+
      geom_ribbon(aes(ymin = 0.9, ymax = 1.1), fill = "grey70", alpha = .3, colour = NA)+
      geom_hline(yintercept=1, linetype='dashed', color="grey35")+
      theme_minimal()+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("Rate app counts/app days")+
      xlab("Appointment month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%y")+
      labs(title=paste0("Rate of appointment counts/appointment days by month"),
           colour= "Dataset")+
      theme(plot.title = element_text(hjust = 0.5, size = 25))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales="free_y")+
      theme(panel.spacing.x= unit(-1, "lines"),
            panel.spacing.y = unit(0, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=12, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15),
            legend.title=element_text(size=17))
    
    
    fig2=ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(appointments_dir,
                    '/plot_rate_app_days_app_count',
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    
    
    
}

