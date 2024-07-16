#######################################################.
#### Publication - make DNA rate bar chart by SIMD ####.
#######################################################.

# Author: Bex Madden
# Date: 2024-07-16


create_bar_chart_dna_simd <- function(dataset_choice){
  
  last_qt_dna <- read_parquet(paste0(apps_att_dir, "apps_att_latest_qt_DNA_forplot.parquet"))
  
  dna_simd_plot_data <- last_qt_dna |> 
    filter(dataset_type == dataset_choice,
           !is.na(simd2020_quintile)) |> 
    mutate(simd2020_quintile = as.factor(simd2020_quintile),
           simd2020_quintile = fct_recode(simd2020_quintile,
                                          "1 - \nMost deprived" = "1",
                                          "5 - \nLeast deprived" = "5"))
  
  lims = round_any(max(dna_simd_plot_data$Percent) + 3, 2.5) # set upper limit of y axis
  
  ggplot(dna_simd_plot_data, aes(x = simd2020_quintile, y = Percent, fill = simd2020_quintile)) +
    geom_bar(stat = "identity") +
    scale_fill_discrete_phs() +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 2.5),
                       labels = function(x) paste0(x,"%")) +
    # labs(
    #   title = paste0("DNA rate for first contact appointments, \nNHSScotland by SIMD deprivation quintile"), #titles will be in markdown
    #   subtitle = paste0("Quarter ending ", month_label)) +
    xlab("SIMD Quintile") +
    ylab("First contact DNA rate") +
    theme_minimal() + #theme_phs()?
    theme(plot.title = element_text(hjust = 0, face = "bold", size = 12,
                                    colour = "#6C2383"),
          plot.subtitle = element_text(size = 10, color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(),
          panel.border = element_rect(colour = "grey90", fill = NA, linewidth = 0.75),
          legend.position = "none",
          axis.title.x = element_text(size = 8, face = "bold",
                                      margin = margin(t = 10)),
          axis.title.y = element_text(size = 8, face = "bold",
                                      margin = margin(t = 10)),
          axis.text.x = element_text(size = 7.5, color = "black"),
          axis.text.y = element_text(size = 7, color = "black"),
          axis.ticks = element_line(colour = "grey90"))
  
  
  ggsave(paste0(apps_att_dir, "dna_simd_plot_last_qt_", dataset_choice, ".png"), 
         height = 10, width = 12, units = "cm")
  
}

