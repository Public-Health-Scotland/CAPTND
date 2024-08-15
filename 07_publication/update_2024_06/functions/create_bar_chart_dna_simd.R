#######################################################.
#### Publication - make DNA rate bar chart by SIMD ####.
#######################################################.

# Author: Bex Madden
# Date: 2024-07-16


create_bar_chart_dna_simd <- function(dataset_choice){
  
  last_qt_dna <- read_parquet(paste0(apps_att_dir, "apps_att_qt_hb_simd.parquet")) |> ## uses output from summarise_appointments_att_WORKING currently
    ungroup() |> 
    select(-total_apps) |> 
    filter(Attendance == "Patient DNA",
           app_quarter_ending == max(app_quarter_ending),
           hb_name == "NHS Scotland") 

  
  dna_simd_plot_data <- last_qt_dna |> 
    filter(dataset_type == dataset_choice,
           !is.na(simd2020_quintile)) |> 
    mutate(simd2020_quintile = as.factor(simd2020_quintile),
           simd2020_quintile = fct_recode(simd2020_quintile,
                                          "1 - \nMost deprived" = "1",
                                          "5 - \nLeast deprived" = "5"))
  
  lims = round_any(max(dna_simd_plot_data$prop_firstcon_att) + 3, 2.5) # set upper limit of y axis
  
  ggplot(dna_simd_plot_data, aes(x = simd2020_quintile, y = prop_firstcon_att, fill = simd2020_quintile)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(prop_firstcon_att, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_discrete_phs() +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "SIMD Quintile",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  
  ggsave(paste0(apps_att_dir, "dna_simd_plot_last_qt_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}

