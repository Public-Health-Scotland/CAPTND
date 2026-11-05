#######################################################.
#### Publication - make referrals bar chart by sex ####.
#######################################################.

# Author: Luke Taylor
# Date: 2026-11-04
# Can now switch between referral and rejected referral outputs using referral_type
# referrals or rej_referrals

create_bar_chart_refs_sex <- function(dataset_choice, referral_type){
  
  last_qt_ref <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/", referral_type, "_quarter_hb_sex.parquet")) |> ## uses output from summarise_referrals
    ungroup() |> 
    select(-total) |> 
    filter(quarter_ending == max(quarter_ending),
           !!sym(dataset_type_o) == dataset_choice,
           !!sym(hb_name_o) == "NHS Scotland",
           !is.na(sex_reported)) 
  
  axis_label <- if_else(
    referral_type == "referrals",
    "Referrals",
    "Rejected Referrals"
  )
  
  lims <- ifelse(dataset_choice == "PT" & referral_type == "referrals", 20000,
    ceiling(max(last_qt_ref$count) / 500) * 500)
  
  y_breaks <- ifelse(dataset_choice == "PT" & referral_type == "referrals",
                     2000, 500)
  
  
  ggplot(last_qt_ref, aes(x = !!sym(sex_reported_o), y = count)) +
    geom_bar(stat = "identity", width = 0.75, fill = "#3F3685") +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, y_breaks),
                       labels = scales::label_comma()) + 
    labs(
      x = "Sex reported",
      y = paste0("Total ", axis_label),
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  chart_width <- 18
  chart_height <- 12
  
  ggsave(paste0(shorewise_pub_data_dir, "/referrals/", referral_type, "_sex_plot_last_qt_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



