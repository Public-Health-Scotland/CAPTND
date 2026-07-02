##########################################################################.
#### DNA Focused Publication - First Contact DNA Chart by Wait Length ####.
##########################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_dna_wait_length <- function(dataset_choice){
  
  last_pub_period_dna_wait_length <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/firstcon_dnas_qt_hb_wait.parquet")) |> 
    ungroup() |> 
    select(-prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, wait_cat) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), wait_cat) |>
    mutate(firstcon_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA')
  
  plot_data <- last_pub_period_dna_wait_length |> 
    filter(!!sym(dataset_type_o) == dataset_choice) 
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(plot_data, aes(x = wait_cat, y = att_rate, fill = wait_cat)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(att_rate, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_discrete_phs(palette = 1) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Wait in weeks for first contact appointment",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_wait_length_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



