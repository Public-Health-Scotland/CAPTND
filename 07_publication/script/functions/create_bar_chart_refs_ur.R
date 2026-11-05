#####################################################################.
#### Publication - make referrals bar chart by urban/rural class ####.
#####################################################################.

# Author: Luke Taylor
# Date: 2026-11-04
# Can now switch between referral and rejected referral outputs using referral_type
# referrals or rej_referrals

create_bar_chart_refs_ur <- function(dataset_choice, referral_type){
  
  last_qt_ref <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/", referral_type, "_quarter_hb_ur.parquet")) |> ## uses output from summarise_referrals
    ungroup() |> 
    select(-total) |> 
    filter(quarter_ending == max(quarter_ending),
           !!sym(hb_name_o) == "NHS Scotland",
           !is.na(ur8_2022_name),
           !!sym(dataset_type_o) == dataset_choice)
  
  axis_label <- if_else(
    referral_type == "referrals",
    "Referrals",
    "Rejected Referrals"
  )
  
  lims = ifelse(dataset_choice == "CAMHS", 
                round_any(max(last_qt_ref$count) + 500, 50), 
                round_any(max(last_qt_ref$count) + 850, 50))  # set upper limit of y axis
  
  ggplot(last_qt_ref, aes(x = ur8_2022_name, y = count)) +
    geom_bar(stat = "identity", width = 0.75, fill = "#3F3685") +
    geom_text(aes(label = paste0(scales::comma(count), "\n(", prop, "%)")), hjust = 0.5, vjust = -0.2, size = 10/.pt) + 
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, ifelse(dataset_choice == "CAMHS", 500, 1000)),
                       labels = scales::label_comma()) +
    #labels = function(x) paste0(x,"%")) + 
    labs(
      x = "Urban Rural Classification",
      y = paste0("Total ", axis_label),
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1))
  
  chart_height <- 16
  chart_width <- 20
  
  ggsave(paste0(shorewise_pub_data_dir, "/referrals/", referral_type, "_ur_plot_last_qt_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



