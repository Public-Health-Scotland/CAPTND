########################################################################.
#### DNA Focused Publication - Total DNA Chart by Location and SIMD ####.
########################################################################.

# Author: Luke Taylor
# Date: 2026-07-03

create_bar_chart_tot_dna_loc_simd <- function(dataset_choice){
  
  last_pub_period_tot_dna_loc_simd <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_qt_hb_loc_simd.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_loc, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, loc_label, simd2020_quintile) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), loc_label, simd2020_quintile) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(loc_label),
           !is.na(simd2020_quintile))
  
  #simd 1
  plot_data_simd1 <- last_pub_period_tot_dna_loc_simd |>
    filter(!!sym(dataset_type_o) == dataset_choice,
           simd2020_quintile == 1) |>
    group_by(dataset_type) |>
    slice_max(order_by = tot_apps, n = 10) |>
    mutate(loc_label = factor(loc_label, levels = unique(loc_label)),
           all_other_appts = tot_apps - apps_att)
  
  lims = round_any(max(plot_data_simd1$tot_apps) + 5000, 2.5) # set upper limit of y axis
  
  
  ggplot(plot_data_simd1, aes(x = loc_label)) +
    geom_col(aes(y = apps_att + all_other_appts, fill = "All other appointments"), show.legend = TRUE) +
    geom_col(aes(y = apps_att, fill = "DNA appointments"), show.legend = TRUE) +
    geom_text(aes(y = apps_att + all_other_appts, label = paste0(att_rate, "%")), hjust = -0.5, size = 10/.pt) +
    scale_fill_manual(name = "Appointments",values = c("All other appointments" = "#3F3685",
                                                       "DNA appointments" = "#AF69A9")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 10000)) +
    labs(x = "Location of planned appointment", 
         y = "Total number of appointments",
         caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1)) +
    coord_flip()
  
  chart_width <- 24
  chart_height <- 10
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_loc_simd1_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
  #simd 5
  plot_data_simd5 <- last_pub_period_tot_dna_loc_simd |>
    filter(!!sym(dataset_type_o) == dataset_choice,
           simd2020_quintile == 5) |>
    group_by(dataset_type) |>
    slice_max(order_by = tot_apps, n = 10) |>
    mutate(loc_label = factor(loc_label, levels = unique(loc_label)),
           all_other_appts = tot_apps - apps_att)
  
  lims = round_any(max(plot_data_simd5$tot_apps) + 5000, 2.5) # set upper limit of y axis
  
  ggplot(plot_data_simd5, aes(x = loc_label)) +
    geom_col(aes(y = apps_att + all_other_appts, fill = "All other appointments"), show.legend = TRUE) +
    geom_col(aes(y = apps_att, fill = "DNA appointments"), show.legend = TRUE) +
    geom_text(aes(y = apps_att + all_other_appts, label = paste0(att_rate, "%")), hjust = -0.5, size = 10/.pt) +
    scale_fill_manual(name = "Appointments",values = c("All other appointments" = "#3F3685",
                                                       "DNA appointments" = "#AF69A9")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 10000)) +
    labs(x = "Location of planned appointment", 
         y = "Total number of appointments",
         caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1)) +
    coord_flip()
  
  chart_width <- 24
  chart_height <- 10
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_loc_simd5_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
  
}



