##################################################################.
#### DNA Focused Publication - Total DNA Chart by Age and Sex ####.
##################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_tot_dna_age_sex <- function(dataset_choice){
  
  last_pub_period_tot_dna_age_sex <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_qt_hb_age.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_age, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(age_group_o), !!sym(sex_reported_o)) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o), !!sym(sex_reported_o)) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(sex_reported),
           age_group != 'Data missing',
           sex_reported != 'Not known')
  
  sex_avg <- tot_dna_rate_sex_avg(dataset_choice = dataset_choice)
  
  plot_data <- last_pub_period_tot_dna_age_sex |> 
    left_join(sex_avg, by = c("dataset_type", "hb_name", "Attendance")) |>
    filter(!!sym(dataset_type_o) == dataset_choice) |>
    mutate(age_group = case_when(
      dataset_type == "CAMHS" ~ as.character(age_group),
      dataset_type == "PT" ~ as.character(age_group)),
      age_group = factor(
        age_group,
        levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                   "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")))
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = age_group, y = att_rate, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    geom_hline(aes(yintercept = unique(plot_data$Female), linetype = 'Female mean'),
               colour = "#AF69A9", linewidth = 0.5) +
    geom_hline(aes(yintercept = unique(plot_data$Male), linetype = 'Male mean'),
               colour = "#3F3685", linewidth = 0.5) +
    # geom_text(aes(label = sprintf("%.1f%%", att_rate)), position = position_dodge(width = 0.75),
    #           hjust = 0.5, vjust = 5.0, colour = "white", size = 8/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_linetype_manual(name = NULL, values = c("Female mean" = "dashed",
                                                  "Male mean" = "dashed")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Age group",
      y = "Total DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = "Sex reported") +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right")
  
  
  chart_height <- 12
  chart_width <- 20
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_agg_age_sex_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}




