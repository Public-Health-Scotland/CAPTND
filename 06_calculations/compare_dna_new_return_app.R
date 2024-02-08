
#author: JBS
#date: 5/2/24



compare_dna_new_return_app <- function() {
  
  df_dna = read_csv_arrow(paste0(dna_dir,'/attendance_status_rates.csv')) %>% 
    filter(count_by_desc=='none'& !!sym(att_status_desc_o)=='DNA') %>% 
    mutate(!!new_or_return_app_o := gsub(" -.*", "", !!sym(new_or_return_app_o))) %>% 
    select(!!dataset_type_o, !!hb_name_o, !!app_month_o, !!new_or_return_app_o, app_count, app_total) %>% 
    group_by(across(all_of(c(dataset_type_o,hb_name_o,app_month_o,new_or_return_app_o)))) %>% 
    mutate(n=sum(app_count),
           total=sum(app_total)) %>% 
    ungroup() %>% 
    select(-c(app_count,app_total)) %>% 
    distinct() %>% 
    mutate(perc_dna=(n*100)/total)
  
  last12months = max(df_dna$app_month, na.rm = T)- months(12)
  
  
  plot_comp_aggreg_captnd_dna <- function(all_dna,ds_type) {
    
    # Remove NAs because in some boards lack of app purpose or attendance status make 
    # it impossible to determine if appointment is new or return
    p2 <- df_dna %>%
      filter(!!sym(dataset_type_o)==ds_type &
               !!sym(app_month_o)>last12months &
               !is.na(!!sym(new_or_return_app_o))) %>%
      ggplot( aes(x=app_month,
                  y=perc_dna,
                  group=new_or_return_app,
                  colour=new_or_return_app,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Type of app: ",new_or_return_app, "<br>",
                    "Appt month: ", gsub('\n','-',app_month), "<br>",
                    "Non attendance (%): ", round(perc_dna,2), "<br>",
                    "Non attendances: ",n, " | Total appointments: ", total
                  ))) +
      geom_line()+
      geom_point()+
      theme_minimal()+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("% non attendances")+
      xlab("Apointment month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%y")+
      labs(title=paste0("Non attendances - New x Return appointments - ",
                        ds_type),
           colour= "")+
      theme(plot.title = element_text(hjust = 0.5, size = 25))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales = 'free_y')+
      theme(panel.spacing.x= unit(-1, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=13, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    
    fig2=ggplotly(p2, tooltip = "text")
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(dna_dir,
                    '/plot_new_return_apps_dna_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  plot_comp_aggreg_captnd_dna(df_dna,'CAMHS')
  plot_comp_aggreg_captnd_dna(df_dna,'PT')
  

}

