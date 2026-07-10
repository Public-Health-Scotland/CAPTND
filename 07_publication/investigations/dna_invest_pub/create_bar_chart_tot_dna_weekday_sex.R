##############################################################.
#### DNA Focused Publication - Total DNA Chart by Weekday ####.
##############################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_tot_dna_weekday_sex <- function(dataset_choice){
  
  last_pub_period_tot_dna_weekday_sex <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_weekday_sex_all_hb.parquet")) |> 
    ungroup() |> 
    select(-prop_apps_att, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o), day_of_week) |>
    mutate(dna_count = sum(dna_count),
           total_apps = sum(total_apps),
           att_rate = round(dna_count/total_apps*100,1)) |>
    filter(!is.na(sex_reported) & sex_reported != 'Not known') |>
    distinct() 
  
  sex_avg <- tot_dna_rate_sex_avg(dataset_choice = dataset_choice)
  
  plot_data <- last_pub_period_tot_dna_weekday_sex |>
    left_join(sex_avg, by = c("dataset_type", "hb_name")) |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = day_of_week, y = att_rate, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    geom_hline(aes(yintercept = unique(plot_data$Female), linetype = 'Female mean'),
               colour = "#AF69A9", linewidth = 0.5) +
    geom_hline(aes(yintercept = unique(plot_data$Male), linetype = 'Male mean'),
               colour = "#3F3685", linewidth = 0.5) +
    geom_text(aes(label = paste0(att_rate, "%")), position = position_dodge(width = 0.75),
              hjust = 0.5, vjust = -1.5, size = 10/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_linetype_manual(name = NULL, values = c("Female mean" = "dashed",
                                                  "Male mean" = "dashed")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Day of week of scheduled appointment",
      y = "Total DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = "Sex reported") +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_weekday_sex_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



