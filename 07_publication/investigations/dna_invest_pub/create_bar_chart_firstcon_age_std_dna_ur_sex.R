#####################################################################################.
#### DNA Focused Publication - First Contact DNA Chart - UR and Sex, Std. by Age ####.
#####################################################################################.

# Author: Luke Taylor
# Date: 2026-07-31

create_bar_chart_firstcon_dna_ur_sex_std_age <- function(dataset_choice){
  
  #bar chart by ur and sex
  plot_data <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/firstcon_dnas_qt_hb_age_std_ur_sex.parquet")) |> 
    ungroup() |> 
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  lims = round_any(max(plot_data$std_rate_by_ur_sex) + 3, 2.5) # set upper limit of y axis
  
  
  ggplot(plot_data, aes(x = ur8_2022_name, y = std_rate_by_ur_sex, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    # geom_text(aes(label = sprintf("%.1f%%", std_rate_by_ur_sex)), position = position_dodge(width = 0.75),
    #           hjust = 0.5, vjust = -0.4, size = 8/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Urban Rural Classification",
      y = "First contact age standardised DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = 'Sex reported') +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1))
  
  chart_height <- 20
  chart_width <- 26
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/age_std_dna_ur_sex_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}




