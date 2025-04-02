

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
  
  
  aggregate=aggregate_CAMHS |> 
    select(-variables_mmi) |>
    rename(!!hb_name_o := HB_new) |>
    mutate(month = as.Date(month)) |>
    correct_hb_names_simple() 
  

  #df_open = read_csv_arrow(paste0(open_cases_dir,'/openCases_subSource.csv')) 
  df_open <- read_csv(paste0(open_cases_dir, '/openCasesCleaned_subSource.csv'))
  all_open = df_open %>% 
    rename(month := sub_month_start,
           n_captnd := count) |>
    full_join(aggregate, by = join_by(!!hb_name_o, !!dataset_type_o, month)) %>%  # full join so na HBs aren't dropped 
    mutate(captnd_perc_agg=round(n_captnd*100/n_aggregate, 2)) 
  
  
  df_plot <- all_open %>% 
    select(!!hb_name_o, !!dataset_type_o, month) %>% 
    distinct() %>% 
    cross_join(all_open %>% 
                 distinct() %>% 
                 mutate(n_captnd = NA,
                        n_aggregate = NA,
                        captnd_perc_agg = NA)) %>% 
    bind_rows(all_open) %>% 
    group_by(across(all_of(c(hb_name_o, dataset_type_o))), month) %>% 
    summarise(n_captnd = sum(n_captnd, na.rm = TRUE),
              n_aggregate = sum(n_aggregate, na.rm = TRUE),
              captnd_perc_agg = sum(captnd_perc_agg, na.rm = TRUE),
              .groups = 'drop')
  
  
  plot_comp_aggreg_captnd_open <- function(df_plot,ds_type) {
    
    p2 <- df_plot %>% 
      filter(!!sym(dataset_type_o) == ds_type) %>% 
      mutate(!!hb_name_o := case_when(!!sym(hb_name_o) == 'NHS Greater Glasgow and Clyde' ~ 'NHS Greater Glasgow\n and Clyde',
                                      TRUE ~ !!sym(hb_name_o))) %>% 
      ggplot(aes(x = hb_name, 
                 y = captnd_perc_agg,
                 text = paste0(
                   "Health Board: ", hb_name, "<br>",
                   "Comparison to aggregate (%): ", round(captnd_perc_agg,2), "<br>",
                   "n CAPTND: ",n_captnd, " | n aggregate: ", n_aggregate
                 ))) +
      geom_bar(position = position_dodge(), stat = "identity") +
      geom_hline(yintercept = 100, linetype = 'dashed', color = "grey35") +
      theme_minimal() +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      scale_fill_manual(values=c("#3F3685",
                                 "#9B4393",
                                 "#0078D4",
                                 "#83BB26")) +
      ylab("% similarity with aggregate") +
      xlab("Healthboard") +
      theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, size=10),
            axis.text.y = element_text(size = 10),
            legend.position = "top",
            plot.caption = element_text(hjust = 0)) +
      labs(title=paste0("Open cases - CAPTND comparison to aggregate (100%) - ",
                        ds_type),
           fill= "Demand type") +
      theme(plot.title = element_text(hjust = 0.5, size = 30),
            plot.subtitle = element_text(hjust = 0.5, size = 14)) +
      theme(legend.position="bottom") +
      theme(panel.spacing = unit(1, "lines")) +
      theme(plot.margin = unit(c(2,2,4,2), "cm"),
            legend.position="bottom",
            panel.spacing = unit(1, "lines"),
            axis.text.x = element_text(size=12, margin = margin(t = 0, r = 0, b = 40, l = 0)),
            axis.text.y = element_text(size = 15, margin = margin(t = 0, r = 0, b = 0, l = 40)),
            axis.title=element_text(size=17),
            legend.text=element_text(size=15),
            legend.title=element_text(size=17))
    
    
    fig2 = ggplotly(p2, tooltip = "text") %>% 
      plotly::layout(annotations = list(x = 1, y = -0.35, text = paste0("<i>Treatment caseload comprises patients who attended at least one treatment appointment and have not been discharged.
Post assessment demand includes all patients who have attended at least 1 appointment of any purpose and have not been discharged.
Total service demand includes all patients whose referrals were accepted and have not been discharged.</i>"), 
showarrow = F, xref='paper', yref='paper', 
xanchor='right', yanchor='auto', xshift=0, yshift=0,
align='right',
font=list(size=17)))
    
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
  
  save_as_parquet(df = df_plot,
                  path = paste0(open_cases_dir, "/comp_data_opencases_CAMHS"))
  
}
