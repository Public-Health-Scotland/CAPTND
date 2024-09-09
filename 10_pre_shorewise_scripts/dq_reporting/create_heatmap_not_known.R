
#####################################################.
### Make Chart Not Known Records, grouped by PMS used ###
#####################################################.

# Author: Charlie Smith
# Date: 2024-05-21

create_heatmap_not_known <- function(df, chart_value){
  
  chart_not_known_pms <- df %>% 
    filter(value == chart_value) %>% 
    mutate(variable = fct_rev(variable),
           hb_name = factor(hb_name, levels = level_order_hb)) %>% 
    ggplot(aes(x = hb_name, y = variable, fill = factor(prop_group))) + 
    geom_tile(width = 1, height = 1, linewidth = .25, color = "black")+ 
    geom_text(aes(label = proportion), size = 2)+
    scale_fill_manual(values = colors_rev, name = "Not Known\nRecords (%)", drop = FALSE)+
    scale_x_discrete(position = "top")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0),
          legend.key = element_rect(fill = "white", colour = "black"),
          plot.caption = element_text(hjust = 0),
          strip.text.y.right = element_text(angle = 0))+
    facet_grid(#`Variable Type` 
      ~ dataset_type #+ PMS
      , scales = "free", space = "free")+
    labs(title = paste0("CAPTND: Not Known Records by HB and DATASET (", month_word_end, " submissions)"),
         subtitle = "Not known records = valid, but not meaningful (e.g. 99, 98, 96)",
         x = NULL,
         y = "Variables", 
         caption = "\n*'Supplementary info' refers to data items that will only apply to a limited number of submitted records.")
  
  return(chart_not_known_pms)

}
