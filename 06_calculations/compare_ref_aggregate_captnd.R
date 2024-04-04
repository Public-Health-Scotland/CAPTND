#########################################################################.
## Compare referral numbers from last aggregate publication and captnd ##
#########################################################################.

#author: JBS
#date: 05/01/24

source("04_check_modify/correct_hb_names_simple.R") 


compare_ref_aggregate_captnd <- function() {
  df_referrals = read_csv_arrow(paste0(referrals_dir,'/referrals.csv')) 
  
  df_referrals_sco <- df_referrals |>
    group_by(!!!syms(c(referral_month_o, dataset_type_o, ref_acc_last_reported_o))) |>
    summarise(n = sum(n, na.rm = TRUE)) |>
    mutate(hb_name = "NHS Scotland") 
  
  df_referrals <- rbind(df_referrals, df_referrals_sco)
  
  getAggregatePatientsSeen <- function(ds_type) {
    ptrn = paste0('PatientsSeen_',ds_type,'_')
    
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
      filter(variables_mmi %in% c('All Referrals Received','Referrals Accepted','ReferralsReceived','ReferralsAccepted')) %>% 
      mutate(!!dataset_type_o := ds_type,
             !!ref_acc_last_reported_o := case_when(variables_mmi=='All Referrals Received' ~ 'total',
                                                    variables_mmi=='Referrals Accepted' ~ 'accepted',
                                                    variables_mmi=='ReferralsReceived' ~ 'total',
                                                    variables_mmi=='ReferralsAccepted' ~ 'accepted')) %>% 
      pivot_longer(starts_with('2'), names_to = 'referral_month', values_to = 'n_aggregate')
    
  }
  
  aggregate_CAMHS= getAggregatePatientsSeen('CAMHS') 
  
  aggregate_PT= getAggregatePatientsSeen('PT')
  
  aggregate=bind_rows(aggregate_CAMHS,aggregate_PT) %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(referral_month = as.Date(referral_month)) %>%
    correct_hb_names_simple() # correct aggregate hb names
  
  
  all_refs = df_referrals %>% 
    filter(!!sym(ref_acc_last_reported_o) %in% c('total', 'accepted'),
           referral_month %in% aggregate$referral_month) %>%
    full_join(aggregate,by = join_by('referral_month', !!hb_name_o, !!dataset_type_o, !!ref_acc_last_reported_o)) %>%  # was inner_join - was causing agg 'accepted' referrals to be dropped if captnd said 'pending'
    mutate(captnd_perc_agg = round(n/n_aggregate*100, 1)) 
  
  

  # plotly plots
  
  plot_comp_aggreg_captnd_ref <- function(all_refs,ds_type) {
    
    p2 <- all_refs %>% 
      filter(!!sym(dataset_type_o)==ds_type) %>% 
      ggplot( aes(x=referral_month, 
                  y=captnd_perc_agg, 
                  group=ref_acc_last_reported, 
                  colour=ref_acc_last_reported,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Referral month: ", gsub('\n','-',referral_month), "<br>",
                    "Referral measure: ", gsub('_',' ',ref_acc_last_reported_o), "<br>",
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
      xlab("Referral month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b\n%y")+
      labs(title=paste0("Referrals - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           colour= "referrals measure")+
      theme(plot.title = element_text(hjust = 0.5, size = 30))+
      facet_wrap(~factor(hb_name, levels=c(level_order)), scales="free_y")+
      theme(panel.spacing.x= unit(-1, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position="bottom",
            axis.text.x = element_text(size=13, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size=15),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15))
    
    
    fig2 = ggplotly(p2, tooltip = "text") 
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = paste0(referrals_dir,
                    '/plot_comp_aggreg_captnd_refs_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
  }
  
  plot_comp_aggreg_captnd_ref(all_refs,'CAMHS')
  
  plot_comp_aggreg_captnd_ref(all_refs,'PT')
  
  ## Save out data
  
  save_as_parquet(df = all_refs, 
                  path = paste0(referrals_dir, "/comp_data_referrals"))
}


