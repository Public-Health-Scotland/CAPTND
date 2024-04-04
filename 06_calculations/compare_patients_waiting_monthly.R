
##############################################################.
### Compare monthly patients waiting - aggregate vs captnd ###
##############################################################.

# Author: Charlie Smith 
# Date: 2024-03-26

compare_patients_waiting_monthly <- function(){
  
  # load aggregate data
  get_aggregate_data <- function(ds_type){
  
  ptrn = paste0('PatientsWaiting_', ds_type,'_')
  
  aggregate_files = list.files(path = '../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                               pattern = ptrn,
                               full.names = FALSE) 
  
  last_date_agg = gsub(ptrn, '', aggregate_files) %>% 
    gsub('.csv', '', .) %>% 
    as.Date(.) %>% 
    max(.) %>% 
    as.character(.)
  
  measure_types <- c("Over 52 weeks unadj Patients waiting", "19 to 35 weeks unadj Patients waiting",
                     "36 to 52 weeks unadj Patients waiting", "Total Patients Waiting unadj",
                     "0 to 18 weeks unadj Patients waiting", "Over 52 weeks unadj Patients waiting", 
                     "Over 18 weeks unadj Patients waiting")
  
  aggregate_data = read_csv_arrow(paste0('../../../../../../MentalHealth3/CAMHS_PT_dashboard/dashboardDataPrep/output/',
                                         ptrn,
                                         last_date_agg,
                                         '.csv'))
  if(ds_type == "PT"){
    
    aggregate_data <- aggregate_data |> 
      mutate(variables_mmi = case_when(
        variables_mmi == "u_TotalPatientsWaiting" ~ "Total Patients Waiting unadj",
        variables_mmi == "u_NumberOfPatientsWaiting0To18Weeks"  ~ "0 to 18 weeks unadj Patients waiting",
        variables_mmi == "u_NumberOfPatientsWaitingOver18Weeks" ~ "Over 18 weeks unadj Patients waiting",
        variables_mmi == "u_NumberOfPatientsWaiting19To35Weeks" ~ "19 to 35 weeks unadj Patients waiting", #combine 
        variables_mmi == "u_NumberOfPatientsWaiting36To52Weeks" ~ "36 to 52 weeks adj Patients waiting", #combine
        variables_mmi == "u_NumberOfPatientsWaitingOver52Weeks" ~ "Over 52 weeks unadj Patients waiting",
        TRUE ~ NA_character_)) |> 
      filter(!is.na(variables_mmi))
    
    }
  
  aggregate_data <- aggregate_data |> 
    rename(hb_name = HB_new) |> 
    correct_hb_names_simple() |> 
    mutate(!!dataset_type_o := ds_type, 
           measure = "patients waiting") %>% 
    pivot_longer(starts_with('2'), names_to = 'app_month', values_to = 'n_aggregate') |> 
    select(measure, measure_type = variables_mmi, dataset_type, hb_name, 
           month = app_month, n_aggregate) |> 
    arrange(dataset_type, hb_name, measure_type) |> 
    filter(measure_type %in% measure_types)
  
  
  return(aggregate_data)
  
  }
  
  df_agg_camhs <- get_aggregate_data(ds_type = "CAMHS")
  df_agg_pt <- get_aggregate_data(ds_type = "PT")
  
  df_agg <- rbind(df_agg_camhs, df_agg_pt) |> 
    mutate(month = ymd(month),
      measure_type = case_when(
      measure_type == "0 to 18 weeks unadj Patients waiting" ~ "0 to 18 weeks",
      measure_type == "19 to 35 weeks unadj Patients waiting" ~ "19 to 52 weeks", # combine "19 to 35 weeks" to "19 to 52 weeks"
      measure_type == "36 to 52 weeks unadj Patients waiting" ~ "19 to 52 weeks", # combine "36 to 52 weeks" to "19 to 52 weeks"
      measure_type == "Over 52 weeks unadj Patients waiting" ~ "Over 52 weeks",
      measure_type == "Over 18 weeks unadj Patients waiting" ~ "Over 18 weeks",
      measure_type == "Total Patients Waiting unadj" ~ "Total waiting",
      TRUE ~ NA_character_)) |> 
    group_by(measure, measure_type, dataset_type, hb_name, month) |> # combine as described above
    summarise(n_aggregate = sum(n_aggregate))
  
  rm(df_agg_camhs, df_agg_pt)
  
    
  # load captnd data
  df_captnd <- read_parquet(paste0(data_export_dir, 
                                   "/patients_waiting/by_month/monthly_waits_patients_waiting_hb.parquet")) |> 
    select(-c(6, 7)) |> 
    mutate(measure = "patients waiting", .before = everything()) |> 
    select(measure, measure_type = wait_group_unadj, dataset_type, hb_name, 
           month = month_start,  n_captnd = waiting_count)
  
  # add "Over 18 weeks"
  df_captnd_over18 <- df_captnd |> 
    filter(measure_type %in% c("19 to 52 weeks", "Over 52 weeks")) |> 
    group_by(measure, dataset_type, hb_name, month) |> 
    summarise(n_captnd = sum(n_captnd)) |> 
    mutate(measure_type = "Over 18 weeks", .after = measure)
  
  # add "Total waiting"
  df_captnd_total_waiting <- df_captnd |> 
    group_by(measure, dataset_type, hb_name, month) |> 
    summarise(n_captnd = sum(n_captnd)) |> 
    mutate(measure_type = "Total waiting", .after = measure)
  
  df_captnd <- df_captnd |> 
    rbind(df_captnd_over18, df_captnd_total_waiting)
  
  
  # join df_agg and df_captnd and calculate percentage similarity
  df_all <- inner_join(df_agg, df_captnd, 
                     by = c("measure", "measure_type", "dataset_type", "hb_name", "month")) |> 
    mutate(captnd_perc_agg = round( n_captnd / n_aggregate * 100, 1)) |> 
    as.data.frame()
  
  
  plot_comp_aggreg_captnd_waiting <- function(df_all, ds_type) {
    
    plot <- df_all %>%
      filter(!!sym(dataset_type_o) == ds_type &
               measure_type %in% c("0 to 18 weeks", "Over 18 weeks")) |> 
      #mutate(captnd_perc_agg = if_else(captnd_perc_agg >= 200, 0, captnd_perc_agg)) |> 
      ggplot(aes(x = month,
                  y = captnd_perc_agg,
                  group = measure_type,
                  colour = measure_type,
                  text = paste0(
                    "Health Board: ", hb_name, "<br>",
                    "Month: ", gsub('\n','-', month), "<br>",
                    "Wait Group: ", measure_type, "<br>",
                    "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                    "n CAPTND: ", n_captnd, " | n aggregate: ", n_aggregate
                  ))) +
      geom_line()+
      geom_point()+
      geom_ribbon(aes(ymin = 90, ymax = 110), fill = "grey70", alpha = .3, colour = NA)+
      theme_minimal()+
      geom_hline(yintercept=100, linetype='dashed', color="grey35")+
      scale_colour_manual(values=c("#3F3685",
                                   "#9B4393"#,
                                   #"#0078D4",
                                   #"#83BB26"
                                   ))+
      ylab("% Similarity to Aggregate Data")+
      xlab("Month")+
      scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b-%y",
        expand = c(0,0))+
      labs(title = paste0(ds_type, " Patients Waiting (Unadjusted) - CAPTND to Aggregate Comparison"),
           colour = "Wait Group")+
      theme(plot.title = element_text(hjust = 0.5, size = 30))+
      facet_wrap(~factor(hb_name, levels = c(level_order_hb)), scales = "free_y")+
      theme(panel.spacing.x= unit(0, "lines"),
            panel.spacing.y = unit(1, "lines"))+
      theme(plot.margin = unit(c(2,2,2,2), "cm"),
            legend.position = "right",
            axis.text.x = element_text(angle = -90, size = 13, hjust = 0, vjust = 0.5,
                                       margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            strip.text = element_text(size = 15),
            axis.title = element_text(size = 17),
            panel.grid.minor = element_blank(),
            legend.title = element_text(size = 17),
            legend.text = element_text(size = 13))
    
    plot_plotly = ggplotly(plot, tooltip = "text")
    
    htmlwidgets::saveWidget(
      widget = plot_plotly, #the plotly object
      file = paste0(patients_waiting_dir,
                    '/plot_comp_aggreg_captnd_waiting_',
                    ds_type,
                    ".html"), #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    
  }
  
  plot_comp_aggreg_captnd_waiting(df_all, ds_type = "CAMHS")
  plot_comp_aggreg_captnd_waiting(df_all, ds_type = "PT")
  
  save_as_parquet(df = df_all, 
                  path = paste0(patients_waiting_dir,'/comp_data_patients_waiting_monthly'))
  
  
}

# To do:
# ~ sym() all colnames

