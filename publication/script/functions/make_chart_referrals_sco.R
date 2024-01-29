
#########################################.
### Create chart - scotland referrals ###
#########################################.

# Author: Charlie Smith
# Date: 2023-11-17

chart_monthly_refs <- function(dataset_choice = c("CAMHS", "PT")){
  
  # get data
  df_refs <- read_parquet(paste0(data_working_safe, 'refs_monthly_sco.parquet')) |> 
    filter(dataset_type == dataset_choice)
  
  # make line chart
  chart_refs_sco <- df_refs |>
    ggplot(aes(x = referral_month, y = referrals))+
      geom_line(color = "#17375E", size = 1.5)+
      geom_point(color = "#17375E", size = 2)+
      xlab("")+
      ylab("Referrals")+
      labs(caption = paste0("Source: CAPTND, ", production_month),
           fill = "")+
      scale_x_date(date_breaks = "1 month", minor_breaks = "1 month", date_labels = "%b-%y",
                   expand = c(0.02, 0.02))+
      scale_y_continuous(breaks = 
                           seq(0, plyr::round_any(max(df_refs$referrals, na.rm = T), 1000, f = ceiling), 
                               ifelse(dataset_choice == "CAMHS", 500, 1000)),
                         minor_breaks = NULL,
                         labels = comma, 
                         limits = c(0, plyr::round_any(max(df_refs$referrals, na.rm = T), 1000,f = ceiling)))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            legend.position = "bottom", 
            plot.caption = element_text(hjust = 1))
  
  
  # save image as PNG
  ggsave(filename = paste0("chart_referrals_", dataset_choice, ".png"),
         plot = chart_refs_sco, 
         device = png,
         path = data_working_safe,
         width = chart_width, 
         height = chart_height,
         units = "cm",
         dpi = 300,
         bg = "white")
  
}




