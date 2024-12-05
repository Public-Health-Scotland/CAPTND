#########################################################.
### Create bar charts to display appointment location ###
#########################################################.

# Author: Bex Madden
# Date: 2024-12-04


create_bar_charts_app_prof <- function(ds = c("CAMHS", "PT")){
  
  # load data
  
  df_prof <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_qt_hb.parquet"))
  
  # plot 
  
  df_prof_plot <- df_prof |> 
    filter(hb_name == "NHS Scotland",
           dataset_type == ds,
           app_quarter_ending == month_end) |> 
    arrange(desc(prop_app_prof)) |> 
    group_by(dataset_type) |> 
    mutate(rank = row_number(),
           top5 = case_when(rank >6 ~ "All other professions",
                            TRUE ~ prof_label)) |> 
    ungroup() |> 
    group_by(top5) |> 
    summarise(n = sum(n), across(), .groups = "drop") |> 
    mutate(prop_top5 = round(n/total_apps*100, 1),
           top5 = replace_na(top5, "Missing data")) |> 
    filter(rank <= 7) |> 
    arrange(rank)
  
  label_order <- df_prof_plot$top5
  
  upper_limit <- max(df_prof_plot$prop_top5)+5
  
  plot_prof <- df_prof_plot |> 
    mutate(top5 = factor(top5, levels = label_order)) |> 
    ggplot(aes(x = fct_rev(top5), y = prop_top5))+
    geom_bar(stat = "identity", fill = "#1E7F84")+
    geom_text(aes(label = paste0(prop_top5, "%")), hjust = -0.1, size = 10/.pt)+
    coord_flip()+
    scale_y_continuous(limits = c(0,upper_limit), breaks = seq(0,upper_limit, by=5)) +
    scale_x_discrete(labels = label_wrap(20)) +
    labs(
      y = "Percentage of total appointments",
      x = "Healthcare professional",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd()+
    theme(panel.grid.major.x = element_line())
  
  
  ggsave(plot = plot_prof, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_apps_by_prof_top5.png"),
         path = paste0(shorewise_pub_data_dir, "/appointments_prof/")) 
  
  
  
  
}