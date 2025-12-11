##################################.
### Product 4 - data retention ###
##################################.

#Author: Luke Taylor
#Date: 2025-12-11

make_product_4 <- function(df, date_max){
  
  date_min <- most_recent_month_in_data %m-% years(1)
  date_max <- most_recent_month_in_data
  
  #opti data
  #df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  df_opti <- df |>
    mutate(sub_month = floor_date(!!sym(header_date_o), unit = "month")) |>
    filter(sub_month <= date_max,
           sub_month > date_min) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), sub_month) |>
    summarise(opti_count = n()) |>
    group_by(!!sym(dataset_type_o), sub_month) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))
  
  #raw data
  df_swift_clean <- read_parquet(paste0(root_dir, "/swift_extract.parquet"))
  
  df_raw <- df_swift_clean |>
    mutate(sub_month = floor_date(!!sym(header_date_o), unit = "month")) |>
    filter(sub_month <= date_max,
           sub_month > date_min,
           hb_name != 'NHS24') |>
    group_by(dataset_type, hb_name, sub_month) |>
    summarise(raw_count = n()) |>
    group_by(!!sym(dataset_type_o), sub_month) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))
  
  df_compare <- df_opti |>
    left_join(df_raw, by = c("dataset_type", "hb_name", "sub_month")) |>
    mutate(perc_retained = round(opti_count/raw_count*100, 1),
           sub_month = as.Date(sub_month))
  
  
  # get df of hb names, dataset types, and the desired time frame
  df_ds_hb <- get_complete_ds_hb(inc_nhs24 = 'FALSE') 
  df_time <- get_time_series(months_lag = 11, time_frame = 'monthly') |> 
    rename(sub_month = month) 
  df_completer <- df_ds_hb |> 
    cross_join(df_time)
  
  # merge in to complete data where no rtt was possible, set up colour formatting for plots
  df_retention_plotting <- df_compare |> 
    right_join(df_completer, by = c('dataset_type', 'hb_name', 'sub_month')) |> 
    
    mutate(perc_retained = replace_na(perc_retained, 0), # fill empty rows with zero for plotting
           traffic_light = case_when(perc_retained > 89.9 ~ '90 to 100%',
                                     perc_retained <= 89.9 & 
                                       perc_retained >= 70 ~ '70 to 89.9%',
                                     perc_retained <70 ~ '0 to 69.9%')) %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order)),
           perc_retained = as.character(perc_retained)) |>
    mutate(display_perc = as.character(perc_retained),
           display_perc = case_when(perc_retained == '0' ~ '-',
                                     TRUE ~ perc_retained)) |>
    change_nhsscotland_label() |>
    mutate(hb_name = factor(hb_name, levels = unique(hb_name)))
  
 
  
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%" = "#B3D7F2", # blue
                             "0 to 69.9%" = "#D26146") # rust 80%
  
  dates <- df_retention_plotting |>
    select(sub_month) |>
    distinct() |>
    mutate(sub_month = as.Date(sub_month)) |>
    pull()
  
  product4_mth_heatmap <- df_retention_plotting %>% 
    
    ggplot(aes(y = factor(!!sym(hb_name_o), levels = rev(unique(!!sym(hb_name_o)))), 
               x = sub_month, fill = traffic_light)) + 
    geom_tile(width = 25, height = 1, linewidth = .25, color = "black")+ 
    geom_text(aes(label = display_perc), size = 2.3)+
    scale_fill_manual(values = traffic_light_colours, 
                      name = '% of retained records', 
                      drop = FALSE,
                      breaks = c("90 to 100%", "70 to 89.9%","0 to 69.9%"))+
    scale_x_date(labels = format(dates, "%b %Y"), breaks = dates)+
    # scale_y_discrete(labels = label_wrap(25))+ # to shorten y axis labels
    theme_minimal()+
    theme(legend.key = element_rect(fill = "white", colour = "black"),
          plot.caption = element_text(hjust = 0))+
    facet_wrap(~ dataset_type)+
    labs(#title = paste0("CAPTND: Percentage of pathways where RTT is possible by healthboard until ", date_max),
      caption=paste0("Source: CAPTND - Date: ", Sys.Date()),
      x = "\nMonth Ending",
      y = NULL)+
    #theme(plot.title = element_text(hjust = 0.5))+
    theme(strip.background = element_rect(fill = "white", linewidth = 1, linetype = "solid"),
          plot.caption = element_text(hjust = 1, size = 6),
          axis.text.x = element_text( hjust = 1, vjust = 1, angle = 45),
          legend.title = element_text(size = 9))
  
  
  ggsave(paste0(opti_report_dir,'/mth_product4_heatmap_', date_max,
                '.png'),
         width=29,
         height=13.5,
         units='cm',
         dpi = 300,
         bg='white')
  

}

