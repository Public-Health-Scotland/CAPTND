################################.
### Product 2 - plot heatmap ###
################################.

#author: JBS
#date: 28/11/23


# 1 Load libraries --------------------------------------------------------

#not needed - loaded previously


# 2 Function --------------------------------------------------------------


product2_plot_heatmap <- function(df_rtt){
  
  pms <- read_csv_arrow('../../../data/hb_sub_system2.csv')
  
  df_rtt_plot_prep_perc <- df_rtt %>%
    select(all_of(data_keys),!!rtt_eval_o) %>% 
    distinct() %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o,rtt_eval_o))) %>% 
    summarise(n=n(),
              .groups='drop') %>% 
    mutate(rtt=case_when(str_detect(!!sym(rtt_eval_o), 'unknown|not possible') ~ 'not possible',
                         TRUE ~ 'possible')) %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o)),rtt) %>% 
    summarise(n=sum(n),
              .groups='drop') %>% 
    group_by(!!!syms(c(hb_name_o,dataset_type_o))) %>% 
    mutate(total=sum(n)) %>% 
    ungroup() %>% 
    mutate(percentage = round(n/total * 100, 1)) %>% 
    filter(rtt == 'possible' ) %>% 
    mutate(traffic_light=case_when(percentage > 89.9 ~ '90 to 100%',
                                   percentage <= 89.9 & 
                                     percentage >= 70 ~ '70 to 89.9%',
                                   percentage <70 ~ '0 to 69.9%')) %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order)),
           a='value',
           percentage=as.character(percentage)) %>% 
    inner_join(pms) 
  
  df_rtt_plot_prep = df_rtt_plot_prep_perc %>% 
    mutate(percentage=sub_system,
           traffic_light='blank',
           a='pms') %>% 
    bind_rows(df_rtt_plot_prep)
  
  
  
  
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%"="#B3D7F2",#blue
                             "0 to 69.9%"="#D26146",#rust 80%
                             "blank"="white") 
  
  
  
  product2_plot_heatmap <- df_rtt_plot_prep %>% 
    ggplot(aes(y = fct_rev(!!sym(hb_name_o)), x = fct_rev(a), fill = traffic_light)) + 
    geom_tile(width = 0.95, height = 0.9, linewidth = .25, color = "black")+ 
    geom_text(aes(label = percentage), size = 3)+
    scale_fill_manual(values = traffic_light_colours, 
                      name = '% of pathways where\nRTT is possible', 
                      drop = FALSE,
                      breaks = c("90 to 100%", "70 to 89.9%","0 to 69.9%"))+
    theme_minimal()+
    theme(
      legend.key = element_rect(fill = "white", colour = "black"),
      plot.caption = element_text(hjust = 0))+
    facet_wrap(~ dataset_type)+
    labs(title = paste0("CAPTND: Percentage of pathways where RTT is possible by healthboard"),
         caption=paste0("Source: CAPTND - Date: ", Sys.Date()),
         x = NULL,
         y = NULL)+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(strip.background = element_rect(
      fill="white", size=1, linetype="solid"),
      plot.caption = element_text(hjust = 1, size = 6)
    )+
    scale_x_discrete(labels = c("",""))
  
  
  ggsave(paste0(product2_dir,'/product2_heatmap.png'),
         width=20,
         height=13.5,
         units='cm',
         dpi = 300,
         bg='white')
}