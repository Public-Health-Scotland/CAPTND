
##########################################.
### Create referral rate by SIMD chart ###
##########################################.

# Author: Charlie Smith
# Date: 2023-11-20

make_chart_ref_rate_simd <- function(dataset_choice = c("CAMHS", "PT")){
  
  # load data 
  df_simd <- read_parquet(file = paste0(data_working_safe, "refs_simd.parquet")) |> 
    filter(simd2020_quintile %in% c("1", "5") &
             dataset_type == dataset_choice)
  
  # Create chart
  pal <- c(
    "1" = "#AF69A9", # 80% magenta
    #"2" = "#CDA1C9", # 50% magenta
    #"3" = "#C5C3DA", # 30% purple
    #"4" = "#9F9BC2", # 50% purple
    "5" = "#655E9D" # 80% purple
  )
  
  chart_ref_simd_ppt <- df_simd %>% 
    ggplot(aes(x = referral_month, y = referral_rate , colour = simd2020_quintile))+
    geom_line(size = 1.5)+
    geom_point(size = 2)+
    xlab("")+
    ylab("Referrals per 1,000 population")+
    labs(caption = paste0("Source: CAPTND, ", production_month),
      colour = "SIMD Quintile")+
    scale_x_date(date_breaks = "1 month", minor_breaks = "1 month", date_labels = "%b-%y")+
    scale_y_continuous(breaks = seq(from = 0, 
                                    to = plyr::round_any(max(df_simd$referral_rate, na.rm = T), 1, ceiling),
                                    by = ifelse(dataset_choice == "CAMHS", 0.5, 0.5)),
                       minor_breaks = NULL,
                       #labels = comma, 
                       limits = c(0, plyr::round_any(max(df_simd$referral_rate, na.rm = T), 1, ceiling)))+
    theme_classic()+
    scale_colour_manual(values = pal, 
                        labels = c("1 = 20% most deprived", 
                                   #"2", "3", "4", 
                                   "5 = 20% least deprived"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
          legend.position = "bottom", 
          plot.caption = element_text(hjust = 1))
  
  
  # save image as PNG
  ggsave(filename = paste0("chart_ref_rate_simd_", dataset_choice, ".png"),
         plot = chart_ref_simd_ppt, 
         device = png,
         path = data_working_safe,
         width = chart_width, 
         height = chart_height,
         units = "cm",
         dpi = 300,
         bg = "white")
  
  
} 

