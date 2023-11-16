
######################.
### Make Product 1 ###
######################.

# Author: Joana Bittencourt Silvestre and Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2023-11-16

#colors as variable
#hb order
#df

library(dplyr)
library(lubridate)
library(arrow)
library(ggplot2)

#read csv of removed rows
df_camhs <- read_csv_arrow(paste0(stats_removed_dir,
                            '/CAMHS_removed_rows_breakdowntable_month.csv')) 
df_pt <- read_csv_arrow(paste0(stats_removed_dir,
                                  '/PT_removed_rows_breakdowntable_month.csv')) 



df <- bind_rows(df_camhs,df_pt) %>% 
  group_by(!!!syms(c(dataset_type_o,hb_name_o,submission_date_o))) %>% 
  summarise(total_rows_sum=sum(!!sym(total_rows_o)),
            remaining_rows_sum=total_rows_sum-(sum(removed_rows)),
         .groups = 'keep') %>% 
  mutate(remaining_rows_perc=round(remaining_rows_sum/total_rows_sum*100, 1)) %>% 
  ungroup() %>% 
  mutate(traffic_light=case_when(remaining_rows_perc > 89.9 ~ '90 to 100%',
                                 remaining_rows_perc <= 89.9 & 
                                   remaining_rows_perc >= 70 ~ '70 to 89.9%',
                                 remaining_rows_perc <70 ~ '0 to 69.9%')) %>% 
  filter(!!sym(submission_date_o)>max(!!sym(submission_date_o))-months(12))
  

traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                           "70 to 89.9%"="#B3D7F2",#blue
                           "0 to 69.9%"="#D26146") #rust 80%



  chart_known <- df %>% 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order))) %>%  
    ggplot(aes_string(y = hb_name_o, x = submission_date_o, fill = 'traffic_light')) + 
    geom_tile(width = 20, height = 1, size = .25, color = "black")+ 
    geom_text(aes(label = remaining_rows_perc), size = 2)+
    scale_fill_manual(values = traffic_light_colours, name = 'remaining rows (%)', drop = FALSE)+
    scale_x_date(#position = "top",
                 date_breaks = "1 month",
                 date_labels = "%b\n%y")+
    theme(#axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),
          legend.key = element_rect(fill = "white", colour = "black"),
          plot.caption = element_text(hjust = 0))+
    facet_wrap(~ dataset_type)+
    labs(title = paste0("CAPTND: remaining rows by HB and DATASET by month"),
         subtitle = "something",
         x = NULL,
         y = NULL)
  

  chart_known


