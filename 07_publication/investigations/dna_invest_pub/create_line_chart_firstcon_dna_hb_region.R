########################################################################.
#### DNA Focused Publication - First Contact DNA Chart by HB Region ####.
########################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_line_plot_firstcon_dna_region <- function(dataset_choice){
  
  last_pub_period_dna_region <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/firstcon_dnas_mth_hb_agg_age_group.parquet")) |> 
    ungroup() |> 
    add_hb_region() |> 
    filter(!!sym(hb_name_o) != "NHS Scotland") |>
    select(-prop_apps_att, -total_age, -agg_age_groups, -hb_name) |> 
    group_by(!!sym(dataset_type_o), hb_region, Attendance, app_month) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), hb_region, app_month) |>
    mutate(firstcon_apps = sum(apps_att),
           att_rate = round(apps_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA')
  
  
  plot_data <- last_pub_period_dna_region |> 
    filter(!!sym(dataset_type_o) == dataset_choice) 
  
  dates <- plot_data |> # make date labels for quarters
    select(!!sym(app_month_o)) |>
    unique() |>
    pull()
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = app_month, y = att_rate, colour = hb_region)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = phs_colors(c("phs-purple", "phs-magenta", "phs-blue"))) +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Reporting Month",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      colour = "Health board region") +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "top",
          legend.title = element_text(size = 12, face = "bold"),
          legend.box.spacing = unit(0, "cm"),
          legend.key.height = unit(10, "pt"),
          legend.key.width = unit(30, "pt"),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_hb_region_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}


