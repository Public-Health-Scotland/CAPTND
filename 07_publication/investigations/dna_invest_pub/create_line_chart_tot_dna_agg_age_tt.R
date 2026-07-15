########################################################################.
#### DNA Focused Publication - Total DNA Chart by Agg Age Timetrend ####.
########################################################################.

# Author: Luke Taylor
# Date: 2026-07-08

create_line_chart_tot_dna_agg_age_tt <- function(dataset_choice){
  
  last_pub_period_dna_agg_age_tt <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/total_dnas_mth_hb_agg_age_group.parquet")) |> 
    ungroup() |> 
    filter(!!sym(hb_name_o) == "NHS Scotland",
           Attendance == 'Patient DNA',
           agg_age_groups != 'Data missing')
  
  plot_data <- last_pub_period_dna_agg_age_tt |> 
    filter(!!sym(dataset_type_o) == dataset_choice) |>
    mutate(agg_age_groups = case_when(
      dataset_type == "CAMHS" ~ as.character(agg_age_groups),
      dataset_type == "PT" ~ as.character(agg_age_groups)),
      agg_age_groups = factor(
        agg_age_groups,
        levels = c("Under 6", "6-11", "12-15", "Over 15",
                   "Under 25", "25-39", "40-64", "65 plus")))
  
  lims = round_any(max(plot_data$prop_apps_att) + 3, 2.5) # set upper limit of y axis
  
  
  camhs_palette <- c("Under 6" = "#AF69A9", "6-11" = "#3F3685", "12-15" = "#83BB26", "Over 15" = "#0078D4")
  
  pt_palette <- c("Under 25" = "#AF69A9", "25-39" = "#3F3685", "40-64" = "#83BB26", "65 plus"  = "#0078D4")
  
  palette <- if (dataset_choice == "CAMHS") {
    camhs_palette
  } else if (dataset_choice == "PT") {
    pt_palette
  }
  
  
  dates <- plot_data |> # make date labels for quarters
    select(!!sym(app_month_o)) |>
    unique() |>
    pull()
  
  ggplot(plot_data, aes(x = app_month, y = prop_apps_att, colour = agg_age_groups, group = agg_age_groups)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # geom_text(aes(label = paste0(prop_apps_att, "%")), position = position_dodge(width = 0.75),
    #           hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_colour_manual(values = palette) +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Appointment month",
      y = "Total DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      colour = "Age group") +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "top",
          legend.title = element_text(size = 12, face = "bold"),
          legend.box.spacing = unit(0, "cm"),
          legend.key.height = unit(10, "pt"),
          legend.key.width = unit(30, "pt"),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_att/dna_agg_age_tt_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



