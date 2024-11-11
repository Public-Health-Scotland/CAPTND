
######################.
### Make Product 1 ###
######################.

# Author: Joana Bittencourt Silvestre and Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2023-11-16

#colors as variable
#hb order
#df

# library(dplyr)
# library(lubridate)
# library(arrow)
# library(ggplot2)
# library(plotly)

make_product_1 <- function() {
  source("./04_check_modify/add_nhsscotland_label.R")
  #read csv of removed rows
  df_camhs <- read_csv_arrow(paste0(stats_removed_dir,
                              '/CAMHS_removed_rows_breakdowntable_month.csv')) 
  df_pt <- read_csv_arrow(paste0(stats_removed_dir,
                                    '/PT_removed_rows_breakdowntable_month.csv')) 
  
  
  
  df_all <- bind_rows(df_camhs,df_pt) %>% 
    #DON'T remove NHS24
    #filter(!str_detect(!!sym(hb_name_o), '24')) %>% 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, submission_date_o))) %>% 
    summarise(total_rows_sum = sum(!!sym(total_rows_o)),
              remaining_rows_sum = total_rows_sum - (sum(removed_rows)),
           .groups = 'keep') %>% 
    mutate(remaining_rows_perc = round(remaining_rows_sum/total_rows_sum*100, 1)) %>% 
    ungroup() %>% 
    mutate(traffic_light = case_when(remaining_rows_perc > 89.9 ~ '90 to 100%',
                                   remaining_rows_perc <= 89.9 & 
                                     remaining_rows_perc >= 70 ~ '70 to 89.9%',
                                   remaining_rows_perc <70 ~ '0 to 69.9%')) %>%  # added to fix error
    filter(!!sym(submission_date_o) > max(!!sym(submission_date_o)) - months(12))
    
  #df_all <- add_nhsscotland_label(df = df_all) #currently makes a list not a df
  #df_all$hb_name[df_all$hb_name == "NHS Scotland"] <- "NHSScotland"
  
  
  traffic_light_colours <- c("90 to 100%" = "#9CC951", # green 80%
                             "70 to 89.9%" = "#B3D7F2",# blue
                             "0 to 69.9%" = "#D26146") # rust 80%
  
  
  
    product1_plot <- df_all %>% 

      mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order))) %>%  
      ggplot(aes(y = !!sym(hb_name_o), x = !!sym(submission_date_o), fill = traffic_light)) + 

      # mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = rev(level_order)),
      #        !!submission_date_o := ymd(!!sym(submission_date_o))) %>%  
      # ggplot(aes(y = hb_name_o, x = submission_date_o, fill = 'traffic_light')) + 
      geom_tile(width = 20, height = 1, linewidth = .25, color = "black")+ 
      geom_text(aes(label = remaining_rows_perc), size = 2)+
      scale_fill_manual(values = traffic_light_colours, name = 'Retained rows', drop = FALSE)+
      scale_x_date(#position = "top",
                   date_breaks = "1 month",
                   date_labels = "%b\n%y")+
      theme_minimal()+
      theme(#axis.text.x = element_text(angle = 0, vjust = 0, hjust=0),
            legend.key = element_rect(fill = "white", colour = "black"),
            plot.caption = element_text(hjust = 0))+
      facet_wrap(~ dataset_type)+
      labs(#title = paste0("CAPTND: Retained rows after first step of data cleaning by month"),
           #subtitle = "Rows not containing data keys are excluded from further analysis",
           caption = paste0("Source: CAPTND - Date: ", Sys.Date(),
                          '\nData keys are: HB, DATASET, UCPN and CHI (or UPI when CHI is not available)'),
           x = NULL,
           y = NULL)+
      theme(strip.background = element_rect(
               color="grey", fill = "white", linewidth = 1, linetype = "solid"),
            plot.caption = element_text(hjust = 1, size = 6)
           )
    
  
    ggsave(paste0(product1_dir,'/product1.png'),
           width=29,
           height=13.5,
           units='cm',
           dpi = 300,
           bg='white')

}



