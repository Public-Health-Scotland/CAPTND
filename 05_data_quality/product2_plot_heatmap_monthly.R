########################################.
### Product 2 - plot heatmap monthly ###
########################################.

#author: Bex Madden
#date: 16/09/24



# 2 Function --------------------------------------------------------------


product2_plot_heatmap_monthly <- function(df_rtt, date_max){

  df_rtt_plot_prepping <- df_rtt %>%
    filter(!str_detect(!!sym(hb_name_o), '24')) %>%  #remove NHS24
    select(all_of(data_keys), !!rtt_eval_o, !!header_date_o) %>% 
    distinct() %>% 
    mutate(sub_month = floor_date(!!sym(header_date_o), unit = "month")) %>%
    group_by(!!!syms(c(hb_name_o, dataset_type_o, rtt_eval_o)), sub_month) %>% 
    summarise(n = n(),
              .groups='drop') %>% 
    # mutate(
    #   rtt_general = case_when(
    #     str_detect(!!sym(rtt_eval_o), 'seen.*') ~ 'possible',
    #     str_detect(!!sym(rtt_eval_o), '.*waiting.*') ~ 'waiting',
    #     !!sym(rtt_eval_o) %in% c('referral pending', 'referral not accepted') ~ 'referral not accepted',
    #     str_detect(!!sym(rtt_eval_o), 'closed') ~ 'closed before seen',
    #     str_detect(!!sym(rtt_eval_o), 'not possible') ~ 'rtt not possible'
    #   )) %>%
    mutate(rtt_general = case_when(str_detect(!!sym(rtt_eval_o), 'unknown | not possible') ~ 'not possible',
                        TRUE ~ 'possible')) %>%
    save_as_parquet(path = paste0(product2_dir, "/product2_data_monthly_", date_max)) |> # save out monthly data that corresponds to heatmap
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
    filter(rtt_general == 'possible') %>%
    mutate(traffic_light = case_when(percentage > 89.9 ~ '90 to 100%',
                                     percentage <= 89.9 & 
                                       percentage >= 70 ~ '70 to 89.9%',
                                     percentage <70 ~ '0 to 69.9%')) %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order)),
           percentage = as.character(percentage)) 
  

  
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%" = "#B3D7F2", # blue
                             "0 to 69.9%" = "#D26146") # rust 80%

  dates <- df_rtt_plot_prepping |>
    filter(sub_month >=  ymd(date_max) - months(12)) |> 
    select(sub_month) |>
    unique() |>
    pull()

  product2_mth_heatmap <- df_rtt_plot_prepping %>% 
    filter(sub_month >=  ymd(date_max) - months(12)) |> 
    ggplot(aes(y = factor(!!sym(hb_name_o), levels = rev(level_order)), x = sub_month, fill = traffic_light)) + 
    geom_tile(width = 25, height = 1, linewidth = .25, color = "black")+ 
    geom_text(aes(label = percentage), size = 2.3)+
    scale_fill_manual(values = traffic_light_colours, 
                      name = '% of pathways where\nRTT is possible', 
                      drop = FALSE,
                      breaks = c("90 to 100%", "70 to 89.9%","0 to 69.9%"))+
    scale_x_date(labels = format(dates, "%b %Y"), breaks = dates)+
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
  
  
  ggsave(paste0(product2_dir,'/mth_product2_heatmap_', date_max,
                '.png'),
         width=29,
         height=13.5,
         units='cm',
         dpi = 300,
         bg='white')
}