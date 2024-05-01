################################.
### Product 2 - plot heatmap ###
################################.

#author: JBS
#date: 28/11/23


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------


product2_plot_heatmap_quarterly <- function(df_rtt, date_max){
  
  subs_quarter_ending_o <- "subs_quarter_ending"
  rtt_o <- "rtt"
  
  df_rtt_plot_prepping <- df_rtt %>%
    filter(!str_detect(!!sym(hb_name_o), '24')) %>%  #remove NHS24
    select(all_of(data_keys), !!rtt_eval_o, !!header_date_o) %>% 
    distinct() %>% 
    mutate(subs_quarter = ceiling_date(!!sym(header_date_o), unit = "quarter") - 1,
           subs_quarter_ending = floor_date(subs_quarter, unit = "month")) %>%
    group_by(!!!syms(c(hb_name_o, dataset_type_o, rtt_eval_o, subs_quarter_ending_o))) %>% 
    summarise(n = n(),
              .groups='drop') %>% 
    mutate(rtt = case_when(str_detect(!!sym(rtt_eval_o), 'unknown | not possible') ~ 'not possible',
                         TRUE ~ 'possible')) %>% 
    group_by(!!!syms(c(hb_name_o, dataset_type_o, subs_quarter_ending_o, rtt_o))) %>% 
    summarise(n = sum(n),
              .groups ='drop') %>% 
    ungroup()
  
  df_rtt_plot_prep_scot <- df_rtt_plot_prepping %>% #make scotland count
    group_by(dataset_type, subs_quarter_ending, rtt) %>% 
    summarise(n = sum(n), .groups = "drop") %>%
    mutate(hb_name = "NHS Scotland") %>%
    ungroup() 
  
  df_rtt_plot_prep_perc <- bind_rows(df_rtt_plot_prepping, df_rtt_plot_prep_scot)%>% #bind in scotland
    group_by(!!!syms(c(hb_name_o, dataset_type_o, subs_quarter_ending_o))) %>% 
    mutate(total = sum(n)) %>% 
    ungroup() %>% 
    mutate(percentage = round(n/total * 100, 1)) %>% #make percentage of total subs where rtt was 'possible'
    filter(rtt == 'possible' ) %>% 
    mutate(traffic_light = case_when(percentage > 89.9 ~ '90 to 100%',
                                   percentage <= 89.9 & 
                                     percentage >= 70 ~ '70 to 89.9%',
                                   percentage <70 ~ '0 to 69.9%')) %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order)),
           percentage = as.character(percentage)) 

  
  
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%" = "#B3D7F2", # blue
                             "0 to 69.9%" = "#D26146") # rust 80%


  product2_plot_heatmap <- df_rtt_plot_prep_perc %>% 
    ggplot(aes(y = factor(!!sym(hb_name_o), levels = rev(level_order)), x = subs_quarter_ending, fill = traffic_light)) + 
    geom_tile(width = 85, height = 0.85, linewidth = .25, color = "black")+ 
    geom_text(aes(label = percentage), size = 3)+
    scale_fill_manual(values = traffic_light_colours, 
                      name = '% of pathways where\nRTT is possible', 
                      drop = FALSE,
                      breaks = c("90 to 100%", "70 to 89.9%","0 to 69.9%"))+
    scale_x_date(date_breaks = "3 month",
                 date_labels = "%b %y")+
    theme_minimal()+
    theme(legend.key = element_rect(fill = "white", colour = "black"),
          plot.caption = element_text(hjust = 0))+
    facet_wrap(~ dataset_type)+
    labs(#title = paste0("CAPTND: Percentage of pathways where RTT is possible by healthboard until ", date_max),
         caption=paste0("Source: CAPTND - Date: ", Sys.Date()),
         x = "\nQuarter Ending",
         y = NULL)+
    #theme(plot.title = element_text(hjust = 0.5))+
    theme(strip.background = element_rect(fill = "white", linewidth = 1, linetype = "solid"),
          plot.caption = element_text(hjust = 1, size = 6),
          axis.text.x = element_text(hjust = 0.9, vjust = 0.5))
  
  
  ggsave(paste0(product2_dir,'/qt_product2_heatmap_', date_max,
                '.png'),
         width=29,
         height=13.5,
         units='cm',
         dpi = 300,
         bg='white')
}