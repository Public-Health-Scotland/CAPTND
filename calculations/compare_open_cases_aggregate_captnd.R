


compare_open_cases_aggregate_captnd <- function() {
  
  
  getAggregateOpenCases <- function(ds_type) {
    ptrn=paste0('PatientsWaiting_',ds_type,'_')
    
    #read all files that have patients seen
    aggregate_files =list.files(path = '../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                                pattern = ptrn,
                                full.names = FALSE)
    
    last_date_agg = gsub(ptrn, '', aggregate_files) %>% 
      gsub('.csv', '', .) %>% 
      as.Date(.) %>% 
      max(.) %>% 
      as.character(.)
    
    aggregate_data=read_csv_arrow(paste0('../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                                         ptrn,
                                         last_date_agg,
                                         '.csv')) %>% 
      filter(variables_mmi %in% c('OpenCases')) %>% 
      mutate(!!dataset_type_o := ds_type) %>% 
      pivot_longer(starts_with('2'), names_to = 'month', values_to = 'n_aggregate')
    
  }
  
  aggregate_CAMHS= getAggregateOpenCases('CAMHS')
  
  #aggregate_PT= getAggregateOpenCases('PT')
  
  aggregate=aggregate_CAMHS %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(month = as.Date(month)) %>% 
    filter(month == max(month))
  
  
  df_open = read_csv_arrow(paste0(open_cases_dir,'/openCases_subSource.csv')) 
  
  all_open = df_open %>% 
    inner_join(aggregate,by = join_by(!!hb_name_o, !!dataset_type_o)) %>% 
    mutate(captnd_perc_agg=round(n*100/n_aggregate, 2))
  
  
  
  
  
  
  plot_comp_aggreg_captnd_open <- function(all_seen,ds_type) {
    
    p2 <- all_open %>% 
      filter(!!sym(dataset_type_o)==ds_type) %>% 
      ggplot( aes(x=hb_name, 
                  y=captnd_perc_agg, 
                  group=demand_type,
                  fill=demand_type,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Demand type: ", demand_type, "<br>", 
                    "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                    "n CAPTND: ",n, " | n aggregate: ", n_aggregate
                  ))) +
      geom_bar(position=position_dodge(), stat="identity")+
      geom_hline(yintercept=100, linetype='dashed', color="grey35")+
      theme_minimal()+
      scale_fill_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("% similarity with aggregate")+
      xlab("Healthboard")+
      theme(axis.text.x = element_text(#angle = 90, 
                                       vjust = 0.5, hjust = 1, size=10),
            axis.text.y = element_text(size = 10),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      labs(title=paste0("Open cases - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           fill= "")+
      theme(plot.title = element_text(hjust = 0.5, size = 30))+
      theme(legend.position="bottom")+
      theme(panel.spacing = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            panel.spacing = unit(1, "lines"),
            axis.text.x = element_text(size=13, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    
    fig2=ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(open_cases_dir,
                    '/plot_comp_aggreg_captnd_open_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  plot_comp_aggreg_captnd_open(all_waiting,'CAMHS')
  
}

