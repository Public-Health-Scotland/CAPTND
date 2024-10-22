
#####################################################.
### Make Chart Known Records, grouped by PMS used ###
#####################################################.

# Author: Charlie Smith
# Date: 2024-05-08

create_heatmap_known <- function(df, chart_value){
  
  chart_known_pms <- df |>  
    filter(#!is.na(pms) & 
             value == chart_value) |>
    mutate(variable = fct_rev(variable)) |> 
    ggplot(aes(x = hb_name, y = variable, fill = prop_group))+ 
    geom_tile(width = 1, height = 1, linewidth = .25, color = "black")+ 
    geom_text(aes(label = proportion), size = 2)+
    scale_fill_manual(values = colors, name = 'Known\nRecords (%)', drop = FALSE)+
    scale_x_discrete(position = "top")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0),
          legend.key = element_rect(fill = "white", colour = "black"),
          plot.caption = element_text(hjust = 0),
          strip.text.y.right = element_text(angle = 0)#,
          #panel.spacing.x = unit(2, "lines") # does not work - not sure how to increase distance between camhs and pt as reqested
    )+
    facet_grid(#`Variable Type`
      ~ dataset_type #+ pms
      , scales = "free", space = "free")+
    labs(title = paste0("Proportion of 'known' records by health board and dataset type (", month_word_end, " submissions)"),
         subtitle = "Known records = valid and meaningful values",
         x = NULL,
         y = "Variables", 
         caption = "\n*'Supplementary info' refers to data items that will only apply to a limited number of submitted records.")
  
  return(chart_known_pms)
  
}
