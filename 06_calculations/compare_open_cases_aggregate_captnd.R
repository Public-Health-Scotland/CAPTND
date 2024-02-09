


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
  

  aggregate=aggregate_CAMHS %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(month = as.Date(month)) %>% 
    filter(month == max(month))
  
  
  df_open = read_csv_arrow(paste0(open_cases_dir,'/openCases_subSource.csv')) 
  
  all_open = df_open %>% 
    inner_join(aggregate,by = join_by(!!hb_name_o, !!dataset_type_o)) %>% 
    mutate(captnd_perc_agg=round(n*100/n_aggregate, 2)) 
  
  
  
  
  df_plot= all_open %>% 
    select(!!hb_name_o, !!dataset_type_o, month) %>% 
    distinct() %>% 
    cross_join(all_open %>% 
                 select(demand_type) %>% 
                 distinct() %>% 
                 mutate(n=NA,
                        n_aggregate=NA,
                        captnd_perc_agg=NA)) %>% 
    bind_rows(all_open) %>% 
    group_by(across(all_of(c(hb_name_o, dataset_type_o))), demand_type, month) %>% 
    summarise(n=sum(n, na.rm = TRUE),
              n_aggregate=sum(n_aggregate, na.rm = TRUE),
              captnd_perc_agg=sum(captnd_perc_agg, na.rm = TRUE),
              .groups = 'drop')
  
  
  plot_comp_aggreg_captnd_open <- function(df_plot,ds_type) {
    
    p2 <- df_plot %>% 
      filter(!!sym(dataset_type_o)==ds_type) %>% 
      mutate(demand_type=gsub('_',' ',demand_type),
             demand_type = factor(demand_type, levels=c('total service demand', 'post assessment demand', 'treatment caseload')),
             !!hb_name_o := case_when(!!sym(hb_name_o) == 'NHS Greater Glasgow and Clyde' ~ 'NHS Greater Glasgow\n and Clyde',
                                      TRUE ~ !!sym(hb_name_o))) %>% 
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
      theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, size=10),
            axis.text.y = element_text(size = 10),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      labs(title=paste0("Open cases - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           fill= "Demand type")+
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            plot.subtitle = element_text(hjust = 0.5, size = 14))+
      theme(legend.position="bottom")+
      theme(panel.spacing = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,4,2), "cm"),
            legend.position="bottom",
            panel.spacing = unit(1, "lines"),
            axis.text.x = element_text(size=12, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15),
            legend.title=element_text(size=17))
    
    
    fig2=ggplotly(p2, tooltip = "text") %>% 
      plotly::layout(annotations = list(x = 1, y = -0.14, text = paste0("Treatment caseload comprise patients who attended at least one treatment appointment and have not been discharged. 
Post assessment demand include all who have attended at least 1 appointment independently from the purpose and have not been discharged.
Total service demand includes all patients whose referrals were accepted and have not been discharged"), 
                                        showarrow = F, xref='paper', yref='paper', 
                                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                        font=list(size=18)))
                       
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(open_cases_dir,
                    '/plot_comp_aggreg_captnd_open_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  plot_comp_aggreg_captnd_open(df_plot,'CAMHS')
  
}

