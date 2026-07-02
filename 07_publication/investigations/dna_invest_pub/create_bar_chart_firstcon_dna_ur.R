################################################################################.
#### DNA Focused Publication - First Contact DNA Chart by Urban Rural Class ####.
################################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_dna_ur <- function(dataset_choice){
  
  #bar chart by urban rural
  last_pub_period_dna_ur <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/firstcon_dnas_qt_hb_ur.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, ur8_2022_name) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ur8_2022_name) |>
    mutate(firstcon_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(ur8_2022_name))
  
  plot_data <- last_pub_period_dna_ur |>
    filter(!!sym(dataset_type_o) == dataset_choice)
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  
  ggplot(plot_data, aes(x = ur8_2022_name, y = att_rate, fill = ur8_2022_name)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75,
             fill = "#AF69A9") +
    geom_text(aes(label = paste0(att_rate, "%")), position = position_dodge(width = 0.75),
              hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Urban Rural Classification based on postcode at referral",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none",
          axis.text.x = element_text(angle = 35, hjust = 1.1, vjust = 1))
  
  chart_height <- 14
  chart_width <- 22
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_ur_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



