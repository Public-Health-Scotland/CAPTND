
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
           prop = round(count/total_refs*100, 1)) |> 
    filter(dataset_type == ds)


  eth_plot <- df_eth_plot |>
    ggplot(aes(x = eth_group, y = prop)) +
    geom_point(size = 3, color = "#1E7F84") +
    coord_flip() +
    scale_y_sqrt(limits = c(0,100), breaks = seq(0,100, by=10)) +
    scale_x_discrete(labels = label_wrap(20)) +
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

