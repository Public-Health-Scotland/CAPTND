###########################################################################.
#### DNA Focused Publication - First Contact DNA Chart by SIMD and Sex ####.
###########################################################################.

# Author: Luke Taylor
# Date: 2026-06-25

create_bar_chart_dna_simd_sex <- function(dataset_choice){
  
  #bar chart by simd and sex
  last_pub_period_dna_simd_sex <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/firstcon_dnas_qt_hb_simd_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, 
             simd2020_quintile, sex_reported) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), simd2020_quintile, sex_reported) |>
    mutate(firstcon_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(simd2020_quintile),
           sex_reported != 'Not known' & !is.na(sex_reported))
  
  sex_avg <- firstcon_dna_rate_sex_avg(dataset_choice = dataset_choice)
  
  plot_data <- last_pub_period_dna_simd_sex |> 
    left_join(sex_avg, by = c("dataset_type", "hb_name", "Attendance")) |>
    filter(!!sym(dataset_type_o) == dataset_choice,
           !is.na(!!sym(simd_quintile_o))) |> 
    mutate(!!sym(simd_quintile_o) := as.factor(!!sym(simd_quintile_o)),
           !!sym(simd_quintile_o) := fct_recode(!!sym(simd_quintile_o),
                                                "1 - \nMost deprived" = "1",
                                                "5 - \nLeast deprived" = "5")) 
  
  lims = round_any(max(plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  
  ggplot(plot_data, aes(x = simd2020_quintile, y = att_rate, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    geom_hline(aes(yintercept = unique(plot_data$Female), linetype = 'Female mean'),
               colour = "#AF69A9", linewidth = 0.5) +
    geom_hline(aes(yintercept = unique(plot_data$Male), linetype = 'Male mean'),
               colour = "#3F3685", linewidth = 0.5) +
    geom_text(aes(label = paste0(att_rate, "%")), position = position_dodge(width = 0.75),
              hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_linetype_manual(name = NULL, values = c("Female mean" = "dashed",
                                                  "Male mean" = "dashed")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "SIMD Quintile",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = 'Sex reported') +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_simd_sex_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



