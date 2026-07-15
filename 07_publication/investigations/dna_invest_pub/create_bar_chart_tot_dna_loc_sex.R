###########################################################.
#### DNA Focused Publication - Total DNA Location SIMD ####.
###########################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_tot_dna_loc_sex <- function(dataset_choice){
  
  last_pub_period_tot_dna_loc_sex <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_qt_hb_loc_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_loc, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(sex_reported_o), loc_label) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o), loc_label) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(loc_label),
           !is.na(sex_reported) & sex_reported != 'Not known')
  
  sex_avg <- tot_dna_rate_sex_avg(dataset_choice = dataset_choice)
  
  plot_data <- last_pub_period_tot_dna_loc_sex |>
    left_join(sex_avg, by = c("dataset_type", "hb_name", "Attendance")) |>
    filter(!!sym(dataset_type_o) == dataset_choice) |>
    group_by(dataset_type, loc_label) |>
    mutate(all_appts = sum(tot_apps)) |>
    group_by(dataset_type, sex_reported) |>
    slice_max(order_by = all_appts, n = 10) |>
    mutate(loc_label = factor(loc_label, levels = unique(loc_label)))
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = fct_rev(loc_label), y = att_rate, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    geom_hline(aes(yintercept = unique(plot_data$Female), linetype = 'Female mean'),
               colour = "#AF69A9", linewidth = 0.5) +
    geom_hline(aes(yintercept = unique(plot_data$Male), linetype = 'Male mean'),
               colour = "#3F3685", linewidth = 0.5) +
    # geom_text(aes(label = paste0(att_rate, "%")), position = position_dodge(width = 0.75),
    #           hjust = -0.5, size = 8/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_linetype_manual(name = NULL, values = c("Female mean" = "dashed",
                                                  "Male mean" = "dashed")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Location of planned appointment",
      y = "Total DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = 'Sex reported') +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1)) + 
    #scale_y_reverse() +
    coord_flip()
  
  chart_height <- 16
  chart_width <- 18
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_loc_sex_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



