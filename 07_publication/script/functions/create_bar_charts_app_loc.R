#########################################################.
### Create bar charts to display appointment location ###
#########################################################.

# Author: Bex Madden
# Date: 2024-12-04


create_bar_charts_app_loc <- function(ds = c("CAMHS", "PT")){
  
  # load data
  
  # df_loc_ur <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_all_hb_urbrur.parquet"))
  # 
  # # plot of all locations with breakdown by urban/rural groupings
  # 
  # df_ur_plot <- df_loc_ur |> 
  #   pivot_longer(cols = 4:8, names_to = "urban_rural") |> 
  #   filter(hb_name == "NHS Scotland",
  #          dataset_type == ds) |> 
  #   mutate(prop_loc = round(value/loc_total*100, 1),
  #          urban_rural = factor(urban_rural, levels = c("Urban", "Accessible", "Remote", "Very Remote", "Not known")))
  # 
  # plot_ur <- ggplot(df_ur_plot, aes(x = loc_label, y = value, fill = urban_rural))+
  #   geom_bar(stat = "identity")+
  #   coord_flip()+
  #   scale_y_sqrt(limits = c(0,100), breaks = seq(0,100, by=10)) +
  #   theme_captnd()+
  #   theme(panel.grid.major.x = element_line(),
  #         legend.position = "bottom",
  #         legend.title = element_blank(),
  #         axis.text.x = element_text(hjust = 0.25, vjust = 0))
  # 
  # 
  # ggsave(plot = plot_ur, device = "png", bg = "white", 
  #        width = chart_width, height = chart_height, units = "cm", dpi = 300,
  #        filename = paste0(ds, "_apps_by_loc_urbrur.png"),
  #        path = paste0(shorewise_pub_data_dir, "/appointments_loc/"))
  
  
  # plot latest quarter top 5
  
  df_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_qt_hb.parquet"))
  
  df_loc_plot <- df_loc |> 
    filter(hb_name == "NHS Scotland",
           dataset_type == ds,
           app_quarter_ending == month_end) |> 
    arrange(desc(prop)) |> 
    group_by(dataset_type) |> 
    mutate(rank = row_number(),
           top5 = case_when(rank >6 ~ "All other locations",
                            TRUE ~ loc_label)) |> 
    ungroup() |> 
    group_by(top5) |> 
    summarise(count = sum(count), across(), .groups = "drop") |> 
    mutate(prop_top5 = round(count/total_apps*100, 1),
           top5 = replace_na(top5, "Missing data")) |> 
    filter(rank <= 7) |> 
    arrange(rank)
  
  label_order <- df_loc_plot$top5
  
  plot_loc <- df_loc_plot |> 
    mutate(top5 = factor(top5, levels = label_order)) |> 
    ggplot(aes(x = fct_rev(top5), y = prop_top5))+
    geom_bar(stat = "identity", width = bar_width, fill = "#1E7F84")+
    geom_text(aes(label = paste0(prop_top5, "%")), hjust = -0.1, size = 10/.pt)+
    coord_flip()+
    scale_y_sqrt(limits = c(0,100), breaks = seq(0,100, by=10)) +
    scale_x_discrete(labels = label_wrap(20)) +
    labs(
      y = "Percentage of total appointments",
      x = "Care contact location",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd()+
    theme(panel.grid.major.x = element_line(),
          axis.text.x = element_text(hjust = 0.25, vjust = 0))
  
  
  ggsave(plot = plot_loc, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_apps_by_loc_top5.png"),
         path = paste0(shorewise_pub_data_dir, "/appointments_loc/"))
  
}