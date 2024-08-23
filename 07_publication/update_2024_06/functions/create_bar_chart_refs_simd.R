########################################################.
#### Publication - make referrals bar chart by SIMD ####.
########################################################.

# Author: Bex Madden
# Date: 2024-07-16


create_bar_chart_refs_simd <- function(dataset_choice){
  
  last_qt_ref <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/referrals_quarter_hb_simd.parquet")) |> ## uses output from summarise_referrals
    ungroup() |> 
    select(-total) |> 
    filter(quarter_ending == max(quarter_ending),
           hb_name == "NHS Scotland") 

  
  ref_simd_plot_data <- last_qt_ref |> 
    filter(dataset_type == dataset_choice,
           !is.na(simd2020_quintile)) |> 
    mutate(simd2020_quintile = as.factor(simd2020_quintile),
           simd2020_quintile = fct_recode(simd2020_quintile,
                                          "1 - \nMost deprived" = "1",
                                          "5 - \nLeast deprived" = "5"))
  
  lims = ifelse(dataset_choice == "CAMHS", 
                round_any(max(ref_simd_plot_data$count) + 500, 50), 
                round_any(max(ref_simd_plot_data$count) + 850, 50))  # set upper limit of y axis
  
  ggplot(ref_simd_plot_data, aes(x = simd2020_quintile, y = count, fill = simd2020_quintile)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(scales::comma(count), "\n(", prop, "%)")), hjust = 0.5, vjust = -0.2, size = 10/.pt) + 
    scale_fill_discrete_phs(palette = 2) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, ifelse(dataset_choice == "CAMHS", 500, 1000)),
                       labels = comma) +
                       #labels = function(x) paste0(x,"%")) + 
    labs(
      x = "SIMD Quintile",
      y = "Total Referrals",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/referrals/ref_simd_plot_last_qt_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}

