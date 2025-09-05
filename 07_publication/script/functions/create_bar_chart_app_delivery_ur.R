
##################################################################.
#### Publication - face to face versus digital by urban-rural ####.
##################################################################.

# Author: Luke Taylor
# Date: 2025-09-02


# plot of face to face appts versus digital appts over the 15 months of publication period
create_app_delivery_ur_bar <- function(dataset_choice){
  
  app_deliver_ur_plot_data <-  read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_app_delivery_ur_all_hb.parquet")) |> 
    filter(app_delivery == 'Face-to-face' | app_delivery == 'Digital',
           !is.na(ur8_2022_name),
           !!sym(dataset_type_o) == dataset_choice) |>
    group_by(!!sym(dataset_type_o), ur8_2022_name) |>
    mutate(tot = sum(count)) |>
    ungroup() |> 
    mutate(prop = round(count/tot*100, 1))

  
  pal <- c(
    "Face-to-face" = "#AF69A9", # magenta
    "Digital" = "#3F3685" # darkest blue
  )
  
  
  ggplot(app_deliver_ur_plot_data, aes(x = ur8_2022_name, 
                                    y = prop, fill = app_delivery, label = paste0(prop, "%"))) +
    geom_bar(position = 'stack', stat = 'identity') +
    scale_y_continuous(limits = c(0, 100),
                       breaks = seq(0, 100, 20),
                       labels = function(x) paste0(x,"%")) +
    scale_fill_manual(values = pal) +
    geom_text(lsize = 4, position = position_stack(vjust = 0.5),colour = "white") +
    labs(
      fill = "Appointment Delivery Method",
      x = "Urban Rural Classification",
      y = "Proportion of Appointments",
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
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_loc/app_delivery_method_bar", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}

