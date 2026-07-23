##############################################################.
#### DNA Focused Publication - Total DNA Chart by Weekday ####.
##############################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_tot_dna_weekday <- function(dataset_choice){
  
  last_pub_period_tot_dna_weekday <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_weekday_all_hb.parquet")) |> 
    ungroup() |> 
    select(-prop_apps_att, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), day_of_week) |>
    mutate(dna_count = sum(dna_count),
           total_apps = sum(total_apps),
           att_rate = round(dna_count/total_apps*100,1)) |>
    distinct() 
  
  plot_data <- last_pub_period_tot_dna_weekday |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = day_of_week, y = att_rate, fill = day_of_week)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75,
             fill = "#3F3685") +
    geom_text(aes(label = sprintf("%.1f%%", att_rate)), position = position_dodge(width = 0.75),
              hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Day of week of scheduled appointment",
      y = "Total DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_weekday_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}
