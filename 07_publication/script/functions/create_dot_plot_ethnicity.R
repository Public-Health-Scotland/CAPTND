
#########################################################.
### Create dot plot to display referrals by ethnicity ###
#########################################################.

# Author: Bex Madden
# Date: 2024-12-04


create_dot_plot_ethnicity <- function(ds = c("PT", "CAMHS")){
  
  # load data
df_eth <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/referrals_ethnicity_grp_all_qt.parquet"))
 
 ## plot 
  df_eth_plot <- df_eth |>
    filter(ref_quarter_ending == max(ref_quarter_ending)) |>
    group_by(dataset_type) |>
    mutate(total_refs = sum(count),
           prop = round(count/total_refs*100, 1),
           count = format(count, big.mark = ",")) |> 
    filter(dataset_type == ds) |> 
    arrange(desc(count))
  
  df_eth_plot$eth_group <- factor(df_eth_plot$eth_group, levels = c("African (inc Scottish/British)",
                                                          "Asian (inc Scottish/British)",
                                                          "Caribbean or Black",
                                                          "Mixed/Multiple",
                                                          "White",
                                                          "Other ethnic group",
                                                          "Not known"))
  
  # label_order <- sort(as.character(df_eth_plot$eth_group))
  # ifelse(any(label_order == "Not known"), 
  #        label_order <- c(label_order[-which(label_order == "Not known")], "Not known"), "")

  upper_limit <- max(df_eth_plot$prop) + 10
  
  eth_plot <- df_eth_plot |>
    arrange(eth_group) |>
    #mutate(eth_group = factor(eth_group, levels = label_order)) |> 
    ggplot(aes(x = fct_rev(eth_group), y = prop)) +
    geom_point(size = 3.5, color = "#3F3685") + #"#0078D4") + was blue
    geom_text(aes(label = paste0(prop, "% (", trimws(count), ")")), hjust = -0.1, size = 10/.pt)+
    coord_flip() +
    scale_y_sqrt(limits = c(0,101), breaks = seq(0,101, by=10)) +
    scale_x_discrete(labels = scales::label_wrap(20)) +
    labs(
      y = "Percentage of total referrals",
      x = "Ethnic group",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.x = element_line(),
          legend.position = "none",
          axis.text.x = element_text(hjust = 0.25, vjust = 0))

  ggsave(plot = eth_plot, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_refs_by_ethnicity.png"),
         path = paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/"))
  
}

