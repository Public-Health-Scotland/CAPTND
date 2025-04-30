##############################################################################.
## Compare first contact numbers from last aggregate publication and captnd ##
##############################################################################.

#author: JBS
#date: 05/01/24


compare_first_contact_aggregate_captnd <- function() {
  
  
  getAggregateFirstContact <- function(ds_type) {
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
      filter(variables_mmi %in% c('First Contact Appointments adj')) %>% 
      mutate(!!dataset_type_o := ds_type) %>% 
      pivot_longer(starts_with('2'), names_to = 'app_month', values_to = 'n_aggregate')
  
  }
  
  aggregate_CAMHS= getAggregateFirstContact('CAMHS')
  
  #aggregate_PT= getAggregatePatientsSeen('PT')
  
  aggregate=aggregate_CAMHS %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(app_month = as.Date(app_month)) %>%
    correct_hb_names_simple() 
  
  
  df_first_treatment = read_csv_arrow(paste0(first_contact_dir,'/first_treatment.csv')) %>%
    mutate(contact_type = 'first treatment') %>%
    rename(app_month = first_contact_month) %>%
    filter(app_month %in% aggregate$app_month)
  
  #Retired script
  # df_first_contact = read_csv_arrow(paste0(first_contact_dir,'/first_contact.csv')) %>% 
  #   mutate(contact_type = 'first contact') %>% 
  #   rename(app_month = first_contact_month) %>% 
  #   filter(app_month %in% aggregate$app_month)
  
  df_first_contact = read_parquet(paste0(apps_firstcon_dir, '/apps_firstcon_mth_hb.parquet')) |>
    filter(Attendance == 'Attended') |>
    mutate(contact_type = 'first contact') |>
    select(dataset_type, hb_name, app_month, n = first_contact, contact_type) |>
    filter(app_month %in% aggregate$app_month)
  
  
  all_first_treat = df_first_treatment %>% 
    bind_rows(df_first_contact) %>% 
    full_join(aggregate,by = join_by('app_month', !!hb_name_o, !!dataset_type_o)) %>%  #full join so NA boards aren't dropped
    mutate(captnd_perc_agg= round(n/n_aggregate*100, 1))
  
  
  
  
  
  
  plot_comp_aggreg_captnd_first_treat <- function(all_first_treat,ds_type) {
    
    p2 <- all_first_treat %>% 
      filter(!!sym(dataset_type_o)==ds_type) %>% 
      ggplot( aes(x=app_month, 
                  y=captnd_perc_agg, 
                  group=contact_type, 
                  colour=contact_type,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Appointment month: ", gsub('\n','-',app_month), "<br>",
                    "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                    "n CAPTND: ",n, " | n aggregate: ", n_aggregate
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
        date_labels = "%b\n%y")+
      labs(title=paste0("Contacts - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           colour= "")+
      theme(plot.title = element_text(hjust = 0.5, size = 25))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales="free_y")+ #### free_y causes plotly to affect the proportions of the plots in facet_wrap
      theme(panel.spacing.x= unit(0, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=15, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    
    fig2 = ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(first_contact_dir,
                    '/plot_comp_aggreg_captnd_first_cont_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
  
  }
  
  plot_comp_aggreg_captnd_first_treat(all_first_treat,'CAMHS')
  
  save_as_parquet(df = all_first_treat,
                  path = paste0(first_contact_dir, "/comp_data_firstcontact"))
  
}
