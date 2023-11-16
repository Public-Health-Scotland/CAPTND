
######################.
### Make Product 1 ###
######################.

# Author: Joana Bittencourt Silvestre and Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2023-11-16

#colors as variable
#hb order
#df






ggplot_heatmap_known <- function(df, chart_value){
  
  chart_known <- df %>% 
    #filter(Value == chart_value) %>% 
    mutate(#Variable = fct_rev(Variable),
           HB = factor(HB, levels = vec_hb_order_avg)) %>%  #hb order
    ggplot(aes(x = HB, y = Variable, fill = prop_group)) + 
    geom_tile(width = 1, height = 1, size = .25, color = "black")+ 
    geom_text(aes(label = Proportion), size = 2)+
    scale_fill_manual(values = colors, name = 'Known\nRecords (%)', drop = FALSE)+
    scale_x_discrete(position = "top")+
    scale_y_discrete(labels=c("RECEIVED_DATE" = expression(bold(RECEIVED_DATE)), 
                              "APP_DATE" = expression(bold(APP_DATE)),
                              "UNA_START_DATE" = expression(bold(UNA_START_DATE)),
                              "DIAG1" = expression(bold(DIAG1)), 
                              "MEASURES1" = expression(bold(MEASURES1)),
                              "DISCHARGE_DATE" = expression(bold(DISCHARGE_DATE)),
                              parse = TRUE))+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0),
          legend.key = element_rect(fill = "white", colour = "black"),
          plot.caption = element_text(hjust = 0))+
    facet_wrap(~ DATASET)+
    labs(title = paste0("CAPTND: Known Records by HB and DATASET (", month_word_end, " submissions)"),
         subtitle = "Known records = valid and meaningful values",
         x = NULL,
         y = "Variables",
         caption = "NB The variable names in bold have had all NA values removed during data preperation - their values will always be 100%")+
    geom_vline(xintercept = c(1.5, 3.5, 5.5), colour = "white", size = 1)
  
  return(chart_known)
  
}


