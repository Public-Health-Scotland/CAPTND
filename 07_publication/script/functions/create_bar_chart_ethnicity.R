
###############################################################.
### Create bar graph to display referral rates by ethnicity ###
###############################################################.

# Author: Luke Taylor
# Date: 2025-07-29


create_bar_chart_ethnicity <- function(ds = c("PT", "CAMHS")){
  
  # load data
  df_eth <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/referrals_ethnicity_grp_all.parquet"))
  
  ## plot latest quarter
  # df_eth_plot <- df_eth |>
  #   filter(ref_quarter_ending == max(ref_quarter_ending)) |>
  #   group_by(dataset_type) |>
  #   mutate(total_refs = sum(count),
  #          prop = round(count/total_refs*100, 1),
  #          count = format(count, big.mark = ",")) |>
  #   filter(dataset_type == ds) |>
  #   arrange(desc(count))
  
  #plot all pub quarters
  df_eth_plot <- df_eth |>
    filter(dataset_type == ds) |>
    mutate(total_refs = sum(count),
           prop = round(count/total_refs*100, 1),
           count = format(count, big.mark = ",")) |>
    arrange(desc(count))
  
  df_eth_plot$eth_group <- factor(df_eth_plot$eth_group, levels = c("African (inc Scottish/British)",
                                                                    "Asian (inc Scottish/British)",
                                                                    "Caribbean or Black",
                                                                    "Mixed/Multiple",
                                                                    "White",
                                                                    "Other ethnic group",
                                                                    "Not known"))
  
  upper_limit <- max(df_eth_plot$rate_per_1000) + 10
  
  
  eth_plot <- df_eth_plot |>
    arrange(eth_group) |>
    ggplot(aes(x = fct_rev(eth_group), y = rate_per_1000)) +
    geom_bar(stat = "identity", width = bar_width, fill = "#3F3685") + #"#1E7F84")+ was teal
    geom_text(aes(label = paste0(rate_per_1000, " per 1,000 (", trimws(count), ")")), hjust = -0.1, size = 10/.pt)+
    coord_flip() +
    scale_y_continuous(limits = c(0,upper_limit), breaks = seq(0,upper_limit, by=5)) +
    scale_x_discrete(labels = scales::label_wrap(20)) +
    labs(
      y = "Rate of referrals per 1,000 pop.",
      x = "Ethnic group",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.x = element_line(),
          legend.position = "none",
          axis.text.x = element_text(hjust = 0.25, vjust = 0))
  
  chart_width <- 24
  chart_height <- 16
  
  ggsave(plot = eth_plot, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_refs_bar_by_ethnicity.png"),
         path = paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/"))
  
}

