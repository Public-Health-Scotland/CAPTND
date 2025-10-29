###################################################.
#### Publication - face to face versus digital ####.
###################################################.

# Author: Luke Taylor
# Date: 2025-09-02


# plot of face to face appts versus digital appts over the 15 months of publication period
create_app_delivery_bar <- function(dataset_choice){
  
  app_deliver_plot_data <-  read_parquet(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_loc/apps_loc_app_delivery_all_hb.parquet")) |> 
    filter(app_delivery == 'Face-to-face' | app_delivery == 'Digital',
           dataset_type == dataset_choice) |>
    group_by(dataset_type, app_month) |>
    mutate(tot = sum(count)) |>
    ungroup() |> 
    mutate(prop = round(count/tot*100, 1))
  
  dates <- app_deliver_plot_data |> # make date labels for quarters
    select(app_month) |>
    unique() |>
    pull()
  
  lims = round_any(max(app_deliver_plot_data$tot), 1000, f = ceiling) # set upper limit of y axis
  
  pal <- c(
    "Face-to-face" = "#AF69A9", # magenta
    "Digital" = "#3F3685" # darkest blue
  )
  
  
  ggplot(app_deliver_plot_data, aes(x = app_month, 
                                    y = count, fill = app_delivery)) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5000),
                       labels = scales::label_comma()) +
    scale_fill_manual(values = pal) +
    labs(
      fill = "Appointment Delivery Method",
      x = "Month",
      y = "Number of Appointments",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    #theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "top",
          legend.title = element_text(size = 12, face = "bold"),
          legend.box.spacing = unit(0, "cm"),
          legend.key.height = unit(10, "pt"),
          legend.key.width = unit(30, "pt"),
          legend.text = element_text(size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))
  
  chart_height <- 14
  chart_width <- 20
  
  ggsave(paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", data_analysis_latest_date, "/shorewise_publication/data/appointments_loc/app_delivery_method_bar", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}

