##############################################################################.
#### DNA Focused Publication - Total DNA Chart by Location and SIMD Merge ####.
##############################################################################.

# Author: Luke Taylor
# Date: 2026-07-03

create_bar_chart_tot_dna_loc_simd_merge <- function(dataset_choice){
  
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
  
  #simd 1 + simd 5
  plot_data_simd <- last_pub_period_tot_dna_loc_simd |>
    filter(!!sym(dataset_type_o) == dataset_choice) |>
    group_by(dataset_type, loc_label) |>
    mutate(all_appts = sum(tot_apps)) |>
    group_by(dataset_type, simd2020_quintile) |>
    slice_max(order_by = all_appts, n = 10) |> ungroup () |>
    filter(simd2020_quintile == 1 | simd2020_quintile == 5) |>
    mutate(loc_label = factor(loc_label, levels = unique(loc_label)),
           all_other_appts = tot_apps - apps_att) |>
    select(-all_appts) |>
    arrange(loc_label, all_other_appts)
  
  lims = round_any(max(plot_data_simd$tot_apps) + 5000, 2.5) # set upper limit of y axis
  
  
  ggplot(plot_data_simd, aes(x = loc_label)) +
    geom_col(aes(y = apps_att + all_other_appts, fill = paste("All other appointments", simd2020_quintile),
                 group = factor(simd2020_quintile)),
             position = position_dodge(width = 0.9), show.legend = TRUE) +
    geom_col(aes(y = apps_att, fill = paste("DNA appointments", simd2020_quintile),
                 group = factor(simd2020_quintile)),
             position = position_dodge(width = 0.9), show.legend = TRUE) +
    # geom_text(aes(y = apps_att + all_other_appts,
    #               label = ifelse(loc_label %in% c("Other", "Data missing", "Not known"), 
    #                              "", sprintf("%.1f%%", att_rate)), group = factor(simd2020_quintile)),
    #   position = position_dodge(width = 0.9),
    #   hjust = -0.5, size = 8/.pt) +
    scale_fill_manual(name = "Appointments",
                      values = c("DNA appointments 1" = "#3F3685",
                                 "All other appointments 1" = "#AFA7D8",
                                 "DNA appointments 5" = "#AF69A9",
                                 "All other appointments 5" = "#D8A0D0"),
                      labels = c("DNA appointments 1" = "SIMD 1: DNA appointments",
                                 "All other appointments 1" = "SIMD 1: All other appointments",
                                 "DNA appointments 5" = "SIMD 5: DNA appointments",
                                 "All other appointments 5" = "SIMD 5: All other appointments")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 10000),
                       labels = scales::comma) +
    labs(x = "Location of planned appointment", 
         y = "Total number of appointments",
         caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    guides(fill = guide_legend(nrow = 2)) +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "top", legend.justification = "center",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1),
          panel.grid.minor = element_blank()) +
    coord_flip()
  
  chart_height <- 20
  chart_width <- 24
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_loc_simd_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
  #simd 5
  # plot_data_simd5 <- last_pub_period_tot_dna_loc_simd |>
  #   filter(!!sym(dataset_type_o) == dataset_choice,
  #          simd2020_quintile == 5) |>
  #   group_by(dataset_type) |>
  #   slice_max(order_by = tot_apps, n = 10) |>
  #   mutate(loc_label = factor(loc_label, levels = unique(loc_label)),
  #          all_other_appts = tot_apps - apps_att)
  # 
  # lims = round_any(max(plot_data_simd5$tot_apps) + 5000, 2.5) # set upper limit of y axis
  # 
  # ggplot(plot_data_simd5, aes(x = loc_label)) +
  #   geom_col(aes(y = apps_att + all_other_appts, fill = "All other appointments"), show.legend = TRUE) +
  #   geom_col(aes(y = apps_att, fill = "DNA appointments"), show.legend = TRUE) +
  #   geom_text(aes(y = apps_att + all_other_appts, label = sprintf("%.1f%%", att_rate)), 
  #             hjust = -0.5, size = 10/.pt) +
  #   scale_fill_manual(name = "Appointments",values = c("All other appointments" = "#3F3685",
  #                                                      "DNA appointments" = "#AF69A9")) +
  #   scale_y_continuous(limits = c(0, lims),
  #                      breaks = seq(0, lims, 10000),
  #                      labels = scales::comma) +
  #   labs(x = "Location of planned appointment", 
  #        y = "Total number of appointments",
  #        caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
  #   theme_captnd() +
  #   theme(panel.grid.major.y = element_line(),
  #         legend.position = "right",
  #         axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1)) +
  #   coord_flip()
  # 
  # chart_width <- 25
  # chart_height <- 14
  # 
  # ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/tot_dna_loc_simd5_", dataset_choice, ".png"),
  #        bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
  
}



