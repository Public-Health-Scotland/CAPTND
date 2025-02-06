
##############################################################.
### Compare monthly patients waiting - aggregate vs captnd ###
##############################################################.

# Author: Charlie Smith 
# Date: 2024-03-26
# Updated: Luke Taylor 2024-12-23

compare_patients_waiting_monthly <- function() {
  
  
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
  
  aggregate_CAMHS = getAggregatePatientsWaiting('CAMHS')
  
  aggregate_PT = getAggregatePatientsWaiting('PT')
  
  aggregate = bind_rows(aggregate_CAMHS,aggregate_PT) %>% 
    select(-variables_mmi) %>% 
    rename(!!hb_name_o := HB_new) %>% 
    mutate(month = as.Date(month)) %>%
    correct_hb_names_simple()
  
  
  df_waiting <- read_csv_arrow(paste0(patients_waiting_dir, '/nPatients_waiting_subSource_monthly.csv'))
    
  df_waiting <- df_waiting |>
    mutate(wait_group_unadj = case_when(wait_group_unadj == 'wait_0_to_18_weeks' ~ '0-18 weeks',
                                        wait_group_unadj == 'wait_19_to_35_weeks' ~ '19-35 weeks',
                                        wait_group_unadj == 'wait_36_to_52_weeks' ~ '36-52 weeks',
                                        wait_group_unadj == 'over_52_weeks' ~ '53+ weeks')) %>% 
    select(-waiting_total, -waiting_prop) |>
    rename(month := sub_month_start,
           waiting_period := wait_group_unadj,
           n_captnd := count)
  
  all_waiting = df_waiting %>% 
    full_join(aggregate,by = join_by(!!hb_name_o, !!dataset_type_o, month, waiting_period)) %>%  # full join so it doesn't just drop data... doesn't actually affect plots though
    mutate(captnd_perc_agg = round(n_captnd/n_aggregate*100, 1),
           measure = 'patient_waiting')
  
  
  # df_waiting_clean <- read_parquet(paste0(pat_waits_dir, '/patients_wait_cleaned_month_hb.parquet'))
  # 
  # df_waiting_clean <- df_waiting_clean |>
  #   mutate(wait_group_unadj = case_when(wait_group_unadj == 'wait_0_to_18_weeks' ~ '0-18 weeks',
  #                                       wait_group_unadj == 'wait_19_to_35_weeks' ~ '19-35 weeks',
  #                                       wait_group_unadj == 'wait_36_to_52_weeks' ~ '36-52 weeks',
  #                                       wait_group_unadj == 'over_52_weeks' ~ '53+ weeks'))  |>
  #   rename(month := sub_month_start,
  #          waiting_period := wait_group_unadj,
  #          n_captnd := count)
  # 
  # all_waiting = df_waiting_clean %>% 
  #   full_join(aggregate,by = join_by(!!hb_name_o, !!dataset_type_o, month, waiting_period)) %>%  # full join so it doesn't just drop data... doesn't actually affect plots though
  #   mutate(captnd_perc_agg = round(n_captnd/n_aggregate*100, 1),
  #          measure = 'patient_waiting')
  
  
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
                    "n CAPTND: ",n_captnd, " | n aggregate: ", n_aggregate
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
    
    
    fig2 = ggplotly(p2, tooltip = "text") 
    
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
  
  
  save_as_parquet(df = all_waiting,
                  path = paste0(patients_waiting_dir, "/comp_data_patients_waiting_monthly"))
}

# To do:
# ~ sym() all colnames

