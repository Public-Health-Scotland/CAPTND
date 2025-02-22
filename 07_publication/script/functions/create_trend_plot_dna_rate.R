#####################################################.
#### Publication - make DNA rate trend by region ####.
#####################################################.

# Author: Bex Madden
# Date: 2024-07-16


# plot of dna rate over the past 15 months in the 3 HB regions
create_trend_plot_dna_rate <- function(dataset_choice){
  

  ### DNA rate only - monthly by hb_REGION 
  dna_trend_plot_data <-  read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_mth_hb.parquet")) |> 
    add_hb_region() |> 
    filter(!is.na(hb_region),
           Attendance == "Patient DNA") |>
    rename(patient_dna = firstcon_att) |> 
    group_by(!!sym(dataset_type_o), hb_region, !!sym(app_month_o)) |> 
    summarise_at(c("patient_dna", "first_contact"), sum) |> 
    mutate(Percent = round(patient_dna/first_contact*100, 1)) |> 
    ungroup() |> 
    filter(!!sym(dataset_type_o) == dataset_choice,
           !is.na(hb_region))
  
  dates <- dna_trend_plot_data |> # make date labels for quarters
    select(!!sym(app_month_o)) |>
    unique() |>
    pull()
  
  lims = round_any(max(dna_trend_plot_data$Percent), 5, f = ceiling) # set upper limit of y axis
  
  ggplot(dna_trend_plot_data, aes(x = !!sym(app_month_o), 
                                  y = Percent, colour = hb_region)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    scale_colour_manual(values = phs_colors(c("phs-purple", "phs-magenta", "phs-blue"))) +
    labs(
      colour = "Health board region",
      x = "Month",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "top",
          legend.title = element_text(size = 12, face = "bold"),
          legend.box.spacing = unit(0, "cm"),
          legend.key.height = unit(10, "pt"),
          legend.key.width = unit(30, "pt"),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_rate_trend_region_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}
