###############################################################################.
#### DNA Focused Publication - Total DNA Chart - SIMD and Sex, Std. by Age ####.
###############################################################################.

# Author: Luke Taylor
# Date: 2026-07-31

create_bar_chart_tot_dna_simd_sex_std_age <- function(dataset_choice){
  
  #bar chart by simd and sex
  plot_data <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_qt_hb_age_std_simd_sex.parquet")) |> 
    ungroup() |> 
    filter(!!sym(dataset_type_o) == dataset_choice,
           !is.na(!!sym(simd_quintile_o))) |> 
    mutate(!!sym(simd_quintile_o) := as.factor(!!sym(simd_quintile_o)),
           !!sym(simd_quintile_o) := fct_recode(!!sym(simd_quintile_o),
                                                "1 - \nMost deprived" = "1",
                                                "5 - \nLeast deprived" = "5")) 
  
  lims = round_any(max(plot_data$std_rate_by_simd_sex) + 3, 2.5) # set upper limit of y axis
  
  
  ggplot(plot_data, aes(x = simd2020_quintile, y = std_rate_by_simd_sex, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    geom_text(aes(label = sprintf("%.1f%%", std_rate_by_simd_sex)), position = position_dodge(width = 0.75),
              hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "SIMD quintile",
      y = "First contact age standardised DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = 'Sex reported') +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/age_std_dna_simd_sex_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}




