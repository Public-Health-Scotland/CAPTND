
####################################################.
### Create bar charts to display referral source ###
####################################################.

# Author: Luke Taylor
# Date: 2025-01-09


create_bar_charts_ref_source <- function(ds = c("CAMHS", "PT")){
  
  # load data
  df_ref_source <- read_parquet(paste0(ref_source_dir, "ref_source_quarter_hb.parquet"))
  
  # plot 
  
  df_ref_source_plot <- df_ref_source |> 
    filter(hb_name == "NHS Scotland",
           dataset_type == ds,
           quarter_ending == month_end) |> 
    arrange(desc(prop)) |> 
    group_by(dataset_type) |> 
    mutate(rank = row_number(),
           top5 = case_when(rank > 5 ~ "All other referral sources",
                            TRUE ~ ref_source_name)) |> 
    ungroup() |> 
    group_by(top5) |> 
    summarise(count = sum(count), across(), .groups = "drop") |> 
    mutate(prop_top5 = round(count/total*100, 1),
           top5 = replace_na(top5, "Missing data"),
           top5_2 = format(count, big.mark = ",")) |> 
    filter(rank <= 6) |> 
    arrange(rank)
  
  label_order <- df_ref_source_plot$top5
  ifelse(any(label_order == "Not known"), 
         label_order <- c(label_order[-which(label_order == "Not known")], "Not known"), "") # put not known and missing to the bottom of the plot
  ifelse(any(label_order == "Missing data"), 
         label_order <- c(label_order[-which(label_order == "Missing data")], "Missing data"), "")
  
  upper_limit <- max(df_ref_source_plot$prop_top5) + 14
  
  plot_ref_source <- df_ref_source_plot |> 
    mutate(top5 = factor(top5, levels = label_order)) |> 
    ggplot(aes(x = fct_rev(top5), y = prop_top5))+
    geom_bar(stat = "identity", width = bar_width, fill = "#83BB26") + #"#1E7F84")+ was teal
    geom_text(aes(label = paste0(prop_top5, "% (", trimws(top5_2), ")")), hjust = -0.1, size = 10/.pt)+
    coord_flip()+
    scale_y_continuous(limits = c(0,upper_limit), breaks = seq(0,upper_limit, by=10)) +
    scale_x_discrete(labels = scales::label_wrap(20)) +
    labs(
      y = "Percentage of referrals",
      x = "Referral source",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd()+
    theme(panel.grid.major.x = element_line())
  
  
  ggsave(plot = plot_ref_source, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_refs_by_ref_source_top5.png"),
         path = paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/")) 
  
  
  
  
}

