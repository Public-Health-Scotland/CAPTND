#################################################################.
#### DNA Focused Publication - Total DNA Chart by Prof Group ####.
#################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_tot_dna_prof <- function(dataset_choice){
  
  last_pub_period_tot_dna_prof <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_qt_hb_prof.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_prof, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, prof_label) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), prof_label) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(prof_label))
  
  plot_data <- last_pub_period_tot_dna_prof |>
    filter(!!sym(dataset_type_o) == dataset_choice) |>
    group_by(dataset_type) |>
    slice_max(order_by = tot_apps, n = 10) |>
    mutate(prof_label = factor(prof_label, levels = unique(prof_label)))
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = fct_rev(prof_label), y = att_rate, fill = prof_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75,
             fill = "#AF69A9") +
    geom_text(aes(label = paste0(att_rate, "%")), position = position_dodge(width = 0.75),
              hjust = -0.5, size = 10/.pt) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Professional group due to deliver appointment",
      y = "Total DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1)) + 
    #scale_y_reverse() +
    coord_flip()
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_prof_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}

