
#################################.
### Product 2 issues bar plot ###
#################################.

#author: JBS
#date: 28/11/23


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------

product2_plot_issues <- function(df_rtt_plot_prep) {
  
  problem_colours=c( #magenta
    #"#9B4393",
    "#AF69A9",
    "#CDA1C9",
    #reds
    #'#751A04',
    #'#902004',
    "#C73918",
    "#D26146",
    "#E39C8C"#,
    #"#EEC4BA",
    # '#FFE8E2',
    #"#F9EBE8"
  )
  
  
  pr_plt <- df_rtt_plot_prep %>% 
    select(all_of(c(hb_name_o, dataset_type_o,rtt_eval_o)),n) %>% 
    filter(str_detect(!!sym(rtt_eval_o),'not possible')) %>% 
    mutate(!!rtt_eval_o := gsub("rtt.*- ",'',!!sym(rtt_eval_o)),
           !!rtt_eval_o := factor(!!sym(rtt_eval_o), level = c('app with no referral acc',
                                                       'patient had appt and ref is pending',
                                                       'app date but no attendance status',
                                                       'attended app but no purpose',
                                                       'unknown'
           ))) %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o))) %>% 
    mutate(n_total=sum(n)) %>% 
    ungroup() %>% 
    mutate(perc=round(n/n_total*100, 1)) %>% 
    
    ggplot(aes(hb_name, 
               perc, 
               fill=!!sym(rtt_eval_o), 
               group=!!sym(rtt_eval_o),
               text = paste0(
                 "Health Board: ", hb_name, "<br>",
                 "RTT issue: ", !!sym(rtt_eval_o), "<br>",
                 "% pathways: ", perc, "<br>",
                 "n pathways: ", n,"<br>"#,
                 #'PMS: ',sub_system
               )
    )) +
    geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
    #scale_fill_discrete_phs()+
    scale_fill_manual(values=problem_colours)+
    labs(title=paste0("Issues with RTT"),
         fill='Issues', 
         x='health board',
         y='% of pathways') +
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
  
  
  fig_p=ggplotly(pr_plt,tooltip = "text")
  
  pname2=paste0(product2_dir,'/product2_issues',
                '.html')
  
  htmlwidgets::saveWidget(
    widget = fig_p, #the plotly object
    file = pname2, #the path & file name
    selfcontained = TRUE #creates a single html file
  )

}