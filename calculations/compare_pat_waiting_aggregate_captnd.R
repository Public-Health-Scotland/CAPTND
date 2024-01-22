


compare_pat_waiting_aggregate_captnd <- function() {
  
  
  getAggregatePatientsWaiting <- function(ds_type) {
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
      filter(variables_mmi %in% c('0 to 18 weeks unadj Patients waiting',
                                  '19 to 35 weeks unadj Patients waiting',
                                  '36 to 52 weeks unadj Patients waiting',
                                  'Over 52 weeks unadj Patients waiting',
                                  'u_NumberOfPatientsWaiting0To18Weeks',
                                  'u_NumberOfPatientsWaiting19To35Weeks',
                                  'u_NumberOfPatientsWaiting36To52Weeks',
                                  'u_NumberOfPatientsWaitingOver52Weeks'
      )) %>% 
      mutate(!!dataset_type_o := ds_type,
             waiting_period = case_when(variables_mmi=='0 to 18 weeks unadj Patients waiting' ~ '0-18 weeks',
                                        variables_mmi=='19 to 35 weeks unadj Patients waiting' ~ '19-35 weeks',
                                        variables_mmi=='36 to 52 weeks unadj Patients waiting' ~ '36-52 weeks',
                                        variables_mmi=='Over 52 weeks unadj Patients waiting' ~ '53+ weeks',
                                        variables_mmi=='u_NumberOfPatientsWaiting0To18Weeks' ~ '0-18 weeks',
                                        variables_mmi=='u_NumberOfPatientsWaiting19To35Weeks' ~ '19-35 weeks',
                                        variables_mmi=='u_NumberOfPatientsWaiting36To52Weeks' ~ '36-52 weeks',
                                        variables_mmi=='u_NumberOfPatientsWaitingOver52Weeks' ~ '53+ weeks')) %>% 
      pivot_longer(starts_with('2'), names_to = 'month', values_to = 'n_aggregate')
    
  }
  
  aggregate_CAMHS= getAggregatePatientsWaiting('CAMHS')
  
  aggregate_PT= getAggregatePatientsWaiting('PT')
  
  aggregate=bind_rows(aggregate_CAMHS,aggregate_PT) %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(month = as.Date(month)) %>% 
    filter(month == max(month))
  
  
  df_waiting = read_csv_arrow(paste0(patients_waiting_dir,'/patients_waitingTimes_notSeen_subSource.csv')) %>% 
    #remove all negative waiting times
    filter(waitingTime >= 0) %>% 
    mutate(waiting_period = case_when(waitingTime <= 18 ~ '0-18 weeks',
                                      waitingTime >= 19 & waitingTime <= 35 ~ '19-35 weeks',
                                      waitingTime >= 36 & waitingTime <= 52 ~ '36-52 weeks',
                                      waitingTime >= 53  ~ '53+ weeks')) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o), waiting_period) %>% 
    summarise(n=n(), .groups = 'drop')
  
  all_waiting = df_waiting %>% 
    inner_join(aggregate,by = join_by(!!hb_name_o, !!dataset_type_o, waiting_period)) %>% 
    mutate(captnd_perc_agg=round(n*100/n_aggregate, 2))
  
  
  
  
  
  
  plot_comp_aggreg_captnd_waiting <- function(all_seen,ds_type) {
    
    p2 <- all_waiting %>% 
      filter(!!sym(dataset_type_o)==ds_type) %>% 
      ggplot( aes(x=hb_name, 
                  y=captnd_perc_agg, 
                  group=waiting_period,
                  fill=waiting_period,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Waiting period: ", waiting_period, "<br>",
                    "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                    "n CAPTND: ",n, " | n aggregate: ", n_aggregate
                  ))) +
      geom_bar(position=position_dodge(), stat="identity")+
      theme_minimal()+
      scale_fill_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("% similarity with aggregate")+
      xlab("Healthboard")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      labs(title=paste0("Patients waiting (unadjusted) - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           colour= "Waiting period")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~waiting_period, scales="free")+
      theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
      theme(legend.position="bottom")+
      theme(panel.spacing = unit(1, "lines"))
    
    
    fig2=ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(patients_waiting_dir,
                    '/plot_comp_aggreg_captnd_waiting_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  plot_comp_aggreg_captnd_waiting(all_waiting,'CAMHS')
  
  plot_comp_aggreg_captnd_waiting(all_waiting,'PT')
}

