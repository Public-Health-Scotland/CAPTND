#############################################################.
#### Publication - make referrals bar chart by age group ####.
#############################################################.

# Author: Luke Taylor
# Date: 2026-11-04
# Can now switch between referral and rejected referral outputs using referral_type
# referrals or rej_referrals

create_bar_chart_refs_age_grp <- function(dataset_choice, referral_type){
  
  
  last_qt_ref <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/", referral_type, "_quarter_hb_age.parquet")) |> ## uses output from summarise_referrals
    ungroup() |> 
    select(-total) |> 
    filter(quarter_ending == max(quarter_ending),
           !!sym(dataset_type_o) == dataset_choice,
           !!sym(hb_name_o) == "NHS Scotland",
           !is.na(age_group)) 
  
  axis_label <- if_else(
    referral_type == "referrals",
    "Referrals",
    "Rejected Referrals"
  )
  
  lims = ceiling(max(last_qt_ref$count) / 500) * 500
  
  ggplot(last_qt_ref, aes(x = !!sym(age_group_o), y = count)) +
    geom_bar(stat = "identity", width = 0.75, fill = "#3F3685") +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 500),
                       labels = scales::label_comma()) + 
    labs(
      x = "Age group at referral",
      y = paste0("Total ", axis_label),
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  chart_width <- 28
  chart_height <- 16
  
  ggsave(paste0(shorewise_pub_data_dir, "/referrals/", referral_type, "_age_grp_plot_last_qt_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



