################################################################################.
## Compare non attendances numbers from last aggregate publication and captnd ##
################################################################################.

#author: JBS
#date: 24/01/24


compare_dna_aggregate_captnd <- function() {
  
  
  getAggregateDNA <- function(ds_type) {
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
      filter(variables_mmi %in% c('Number of DNAs')) %>% 
      mutate(!!dataset_type_o := ds_type) %>% 
      pivot_longer(starts_with('2'), names_to = 'app_month', values_to = 'n_aggregate')
  
  }
  
  aggregate_CAMHS= getAggregateFirstContact('CAMHS')
  
  #aggregate_PT= getAggregatePatientsSeen('PT')
  
  aggregate=aggregate_CAMHS %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(app_month = as.Date(app_month))
  
  
  df_captnd = read_csv_arrow(paste0(dna_dir,'/attendance_status_rates.csv')) %>% 
    filter(count_by_desc=='none',!!sym(new_or_return_app_o)=='new') %>% 
    select(!!dataset_type_o,!!hb_name_o,!!app_month_o,!!att_status_desc_o,att_status_rate, app_count)
  
  
  #I have to add DNA rate of zero for all months where the seen rate is 100%
  df_dna = df_captnd %>% 
    filter(att_status_desc=='seen' & att_status_rate == 100.0) %>% 
    mutate(att_status_desc='DNA',
           app_count = 0) %>% 
    bind_rows(df_captnd) %>% 
    filter(att_status_desc=='DNA') %>% 
    select(-c(att_status_desc,att_status_rate)) %>% 
    rename(n=app_count)
  
  
  
  
  all_dna = df_dna %>% 
    filter(app_month %in% aggregate$app_month) %>% 
    inner_join(aggregate,by = join_by('app_month', !!hb_name_o, !!dataset_type_o)) %>% 
    mutate(captnd_perc_agg=n*100/n_aggregate)
  
  
  
  
  # 
  # 
  # plot_comp_aggreg_captnd_first_cont <- function(all_first_cont,ds_type) {
  #   
  #   p2 <- all_first_cont %>% 
  #     filter(!!sym(dataset_type_o)==ds_type) %>% 
  #     ggplot( aes(x=app_month, 
  #                 y=captnd_perc_agg, 
  #                 group=ds_type, 
  #                 colour=ds_type,
  #                 text = paste0(
  #                   "Health Board: ", hb_name, "<br>",
  #                   "First contact month: ", gsub('\n','-',app_month), "<br>",
  #                   "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
  #                   "n CAPTND: ",n, " | n aggregate: ", n_aggregate
  #                 ))) +
  #     geom_line()+
  #     geom_point()+
  #     geom_ribbon(aes(ymin = 90, ymax = 110), fill = "grey70", alpha = .3, colour = NA)+
  #     theme_minimal()+
  #     geom_hline(yintercept=100, linetype='dashed', color="grey35")+
  #     scale_colour_manual(values=c("#3F3685",
  #                                  "#9B4393",
  #                                  "#0078D4",
  #                                  "#83BB26"))+
  #     ylab("% similarity with aggregate")+
  #     xlab("First contact month")+
  #     scale_x_date(
  #       date_breaks = "1 month",
  #       date_labels = "%b\n%y")+
  #     labs(title=paste0("First contact - CAPTND comparison to aggregate (100%) - ",
  #                       ds_type),
  #          colour= "")+
  #     theme(plot.title = element_text(hjust = 0.5, size = 25))+
  #     facet_wrap(~factor(hb_name, levels=c(level_order)))+
  #     theme(plot.margin = unit(c(2,2,2,2), "cm"),
  #           legend.position="bottom",
  #           panel.spacing = unit(1, "lines"),
  #           axis.text.x = element_text(size=15, margin = margin(t = 0, r = 0, b = 40, l = 0)),
  #           axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
  #           strip.text = element_text(size=15),
  #           axis.title=element_text(size=17),
  #           legend.text=element_text(size=15))
  #   
  #   
  #   fig2=ggplotly(p2, tooltip = "text") 
  #   
  #   htmlwidgets::saveWidget(
  #     widget = fig2, #the plotly object
  #     file = paste0(first_contact_dir,
  #                   '/plot_comp_aggreg_captnd_first_cont_',
  #                   ds_type,
  #                   ".html"), #the path & file name
  #     selfcontained = TRUE #creates a single html file
  #   )
  # 
  # }
  # 
  # plot_comp_aggreg_captnd_first_cont(all_first_cont,'CAMHS')
  
}
