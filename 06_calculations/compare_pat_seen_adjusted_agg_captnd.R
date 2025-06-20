
#Comparison script based on new patients seen calculation
#check whether '/pat_seen_adj_wait_grp_mth.parquet' already exists
#if it has not been generated yet, then run summarise_patients_seen() on Line 69
#function will take nearly 30 minutes to run

compare_pat_seen_adj_agg_captnd <- function() {
  
  
  getAggregatePatientsSeen <- function(ds_type) {
    ptrn=paste0('PatientsSeen_',ds_type,'_')
    
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
      filter(variables_mmi %in% c('0 to 18 weeks adj Patients seen',
                                  '19 to 35 weeks adj Patients seen',
                                  '36 to 52 weeks adj Patients seen',
                                  'Over 52 weeks adj Patients seen'
      )) %>% 
      mutate(!!dataset_type_o := ds_type,
             waiting_period = case_when(variables_mmi=='0 to 18 weeks adj Patients seen' ~ '0-18 weeks',
                                        variables_mmi=='19 to 35 weeks adj Patients seen' ~ '19-35 weeks',
                                        variables_mmi=='36 to 52 weeks adj Patients seen' ~ '36-52 weeks',
                                        variables_mmi=='Over 52 weeks adj Patients seen' ~ '53+ weeks')) %>% 
      pivot_longer(starts_with('2'), names_to = 'app_month', values_to = 'n_aggregate')
    
  }
  
  aggregate_CAMHS= getAggregatePatientsSeen('CAMHS')
  
  aggregate_PT= getAggregatePatientsSeen('PT')
  
  aggregate=bind_rows(aggregate_CAMHS,aggregate_PT) %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(!!app_month_o := as.Date(!!sym(app_month_o))) %>%
    correct_hb_names_simple() #add in HB name correction
  
  #summarise_patients_seen()
  
  df_seen = read_parquet(paste0(pat_seen_dir,'pat_seen_adj_wait_grp_mth.parquet')) |>
    mutate(adj_rtt_group = case_when(adj_rtt_group == '0 to 18 weeks' ~ '0-18 weeks',
                                     adj_rtt_group == '19 to 35 weeks' ~ '19-35 weeks',
                                     adj_rtt_group == '36 to 52 weeks' ~ '36-52 weeks',
                                     adj_rtt_group == 'Over 52 weeks' ~ '53+ weeks')) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    select(dataset_type, hb_name, app_month = first_treat_month, waiting_period = adj_rtt_group, n_captnd = n)
  
  
  all_seen = df_seen %>% 
    filter(app_month %in% aggregate$app_month) %>% 
    full_join(aggregate,by = join_by('app_month', !!hb_name_o, !!dataset_type_o, waiting_period)) %>%  #use full join to keep everything present in aggregate. Introduces some Infs
    mutate(captnd_perc_agg=round(n_captnd/n_aggregate*100, 1))
  
  
  
  
  
  plot_comp_aggreg_captnd_seen <- function(all_seen,ds_type) {
    
    p2 <- all_seen %>% 
      filter(!!sym(dataset_type_o)==ds_type) %>% 
      ggplot( aes(x=app_month, 
                  y=captnd_perc_agg, 
                  group=waiting_period, 
                  colour=waiting_period,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "App month: ", gsub('\n','-',app_month), "<br>",
                    "Waiting period: ", waiting_period, "<br>",
                    "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                    "n CAPTND: ",n_captnd, " | n aggregate: ", n_aggregate
                  ))) +
      geom_line()+
      geom_point()+
      geom_ribbon(aes(ymin = 90, ymax = 110), fill = "grey70", alpha = .3, colour = NA)+
      theme_minimal()+
      geom_hline(yintercept=100, linetype='dashed', color="grey35")+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393",
                                   "#0078D4",
                                   "#83BB26"))+
      ylab("% similarity with aggregate")+
      xlab("Appointment month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%y",
        expand = c(0,0))+
      labs(title=paste0("Patients seen (unadjusted) - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           colour= "Waiting period")+
      theme(plot.title = element_text(hjust = 0.5, size = 30))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales="free_y")+
      theme(panel.spacing.x= unit(0, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=13, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.title=element_text(size=17),
            legend.text=element_text(size=13))
    
    
    fig2=ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(pat_seen_dir,
                    '/plot_comp_aggreg_captnd_seen_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  fig <- plot_comp_aggreg_captnd_seen(all_seen,'CAMHS')
  
  fig <- plot_comp_aggreg_captnd_seen(all_seen,'PT')
  
  #save out data
  
  save_as_parquet(df = all_seen, 
                  path = paste0(patients_seen_dir, "/comp_data_adj_patientsseen"))
}



