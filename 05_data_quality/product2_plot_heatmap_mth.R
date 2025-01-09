####################################################################################.
### Product 2 rework - plot heatmap of pathways where rtt was 'possible' monthly ###
####################################################################################.

#author: Bex Madden
#date: 01/10/24



# 2 Function --------------------------------------------------------------


product2_plot_heatmap_mth <- function(df_rtt, date_max){
  
  source("./09_ideas_space/get_complete_ds_hb.R") # temporary location
  source("./09_ideas_space/get_time_series.R") # temporary location
  
  
  # get counts of pathways for which rtt was and was not possible, get percentage where it was possible
  df_rtt_monthly <- df_rtt %>%
    filter(!str_detect(!!sym(hb_name_o), '24')) %>%  #remove NHS24
    select(all_of(data_keys), !!rtt_eval_o, !!header_date_o) %>% 
    distinct() %>% 
    mutate(sub_month = floor_date(!!sym(header_date_o), unit = "month")) %>%
    group_by(!!!syms(c(hb_name_o, dataset_type_o, rtt_eval_o)), sub_month) %>% 
    summarise(n = n(),
              .groups='drop') %>% 
    mutate(
      rtt_general = case_when(
        str_detect(!!sym(rtt_eval_o), 'seen.*') ~ 'possible',
        str_detect(!!sym(rtt_eval_o), '.*waiting.*') ~ 'waiting',
        !!sym(rtt_eval_o) %in% c('referral pending', 'referral not accepted') ~ 'referral not accepted',
        str_detect(!!sym(rtt_eval_o), 'closed') ~ 'closed before seen',
        str_detect(!!sym(rtt_eval_o), 'not possible') ~ 'rtt not possible'
      )) %>%
    filter(rtt_general == 'possible' | rtt_general == 'rtt not possible') |> 
    
    save_as_parquet(path = paste0(product2_dir, "/product2_data_monthly_rework_", date_max)) |>  # save out monthly data that corresponds to heatmap
  
    group_by(!!!syms(c(hb_name_o, dataset_type_o)), rtt_general, sub_month) %>% 
    summarise(n = sum(n),
              .groups ='drop') %>% 
    group_by(!!!syms(c(dataset_type_o)), rtt_general, sub_month) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) %>%
    group_by(!!!syms(c(hb_name_o, dataset_type_o)), sub_month) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(percentage = round(n/total * 100, 1)) %>% #make percentage of total subs where rtt was 'possible'
    filter(rtt_general == 'possible',
           sub_month >=  floor_date(date_max, "month") - months(12)) # filter for 12 month time frame
  
  
  
  # get df of hb names, dataset types, and the desired time frame
  df_ds_hb <- get_complete_ds_hb(inc_nhs24 = 'FALSE') 
  df_time <- get_time_series(months_lag = 11, time_frame = 'monthly') |> 
    rename(sub_month = month) 
  df_completer <- df_ds_hb |> 
    cross_join(df_time)
  
  # merge in to complete data where no rtt was possible, set up colour formatting for plots
  df_rtt_plot_prepping <- df_rtt_monthly |> 
    right_join(df_completer, by = c('dataset_type', 'hb_name', 'sub_month')) |> 
    
    mutate(percentage = replace_na(percentage, 0), # fill empty rows with zero for plotting
           traffic_light = case_when(percentage > 89.9 ~ '90 to 100%',
                                     percentage <= 89.9 & 
                                       percentage >= 70 ~ '70 to 89.9%',
                                     percentage <70 ~ '0 to 69.9%')) %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order)),
           percentage = as.character(percentage)) 
  
  #df_rtt_plot_prepping <- add_nhsscotland_label(df = df_rtt_plot_prepping) #currently makes a list not a df
  #df_rtt_monthly$hb_name[df_rtt_monthly$hb_name == "NHS Scotland"] <- "NHSScotland"
  
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%" = "#B3D7F2", # blue
                             "0 to 69.9%" = "#D26146") # rust 80%
  
  dates <- df_rtt_plot_prepping |>
    select(sub_month) |>
    unique() |>
    pull()
  
  product2_mth_heatmap <- df_rtt_plot_prepping %>% 

    ggplot(aes(y = factor(!!sym(hb_name_o), levels = rev(level_order)), x = sub_month, fill = traffic_light)) + 
    geom_tile(width = 25, height = 1, linewidth = .25, color = "black")+ 
    geom_text(aes(label = percentage), size = 2.3)+
    scale_fill_manual(values = traffic_light_colours, 
                      name = '% of pathways where\nRTT is possible', 
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
  
  
  ggsave(paste0(product2_dir,'/mth_product2_heatmap_rework_', date_max,
                '.png'),
         width=29,
         height=13.5,
         units='cm',
         dpi = 300,
         bg='white')
}


