
##################################.
### Product 2 details bar plot ###
##################################.

#author: JBS
#date: 28/11/23


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------


product2_plot_details <- function(df_rtt_plot_prep, date_max) {
  
  
  long_colour_list=c(
    #purples
    "#3F3685",
    "#3F3685",
    "#3F3685",
    "#3F3685",
    
    "#655E9D",
    "#655E9D",
    "#655E9D",
    
    "#9F9BC2",
    "#9F9BC2",
    
    "#C5C3DA",
    "#C5C3DA",
    "#C5C3DA",
    "#C5C3DA",
    
    #magenta
    "#AF69A9",
    "#AF69A9",

    #reds
    '#902004',
    '#902004',
    '#902004',
    '#902004',
    '#902004')
  
  p <- df_rtt_plot_prep %>% 
    ggplot(aes(hb_name, 
               perc, 
               fill=!!sym(rtt_eval_o), 
               group=!!sym(rtt_eval_o),
               text = paste0(
                 "Health Board: ", hb_name, "<br>",
                 "RTT status: ", !!sym(rtt_eval_o), "<br>",
                 "% pathways: ", perc, "<br>",
                 "n pathways: ", n,"<br>"
               )
    )) +
    geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
    scale_fill_manual(values=long_colour_list)+
    labs(title=paste0("Percentage of pathways where RTT is possible - detailed RTT evaluation - until ", date_max),
         fill='RTT status', 
         x='health board',
         y='% pathways') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          plot.caption = element_text(hjust = 0))+
    theme(legend.position="bottom")+
    theme(plot.title = element_text(hjust = 0.5, size=30))+
    facet_wrap(~dataset_type)+
    theme(panel.spacing = unit(1, "lines"))+
    theme(plot.margin = unit(c(2,2,2,2), "cm"),
          legend.position="bottom",
          axis.text.x = element_text(size=15, margin = margin(t = 0, r = 0, b = 40, l = 0)),
          axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
          strip.text = element_text(size=20),
          axis.title=element_text(size=17),
          legend.text=element_text(size=13),
          legend.title=element_text(size=15))
  
  
  fig=ggplotly(p,tooltip = "text")
  
  pname=paste0(opti_report_dir,'/product2_details_',date_max,
               '.html')
  
  htmlwidgets::saveWidget(
    widget = fig, #the plotly object
    file = pname, #the path & file name
    selfcontained = TRUE #creates a single html file
  )
  
  


}
