##############################################################.
#### Publication - make DNA rate bar chart by wait length ####.
##############################################################.

# Author: Luke Taylor
# Date: 2026-05-04


create_bar_chart_dna_wait_length_15mth <- function(dataset_choice){
  
  last_pub_period_dna <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb_wait_length.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -att_rate, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, wait_cat) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), wait_cat) |>
    mutate(firstcon_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA')
  
  
  dna_wait_length_plot_data <- last_pub_period_dna |> 
    filter(!!sym(dataset_type_o) == dataset_choice) 
  
  lims = round_any(max(dna_wait_length_plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(dna_wait_length_plot_data, aes(x = wait_cat, y = att_rate, fill = wait_cat)) +
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
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_wait_length_plot_last_15mth_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



create_bar_chart_dna_age_sex_15mth <- function(dataset_choice){
  
  last_pub_period_dna <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb_age_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending, -app_month) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, agg_age_groups, !!sym(sex_reported_o)) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), agg_age_groups, !!sym(sex_reported_o)) |>
    mutate(firstcon_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !!sym(sex_reported_o) %in% c("Male", "Female"),
           agg_age_groups != 'Data missing')
  
  
  dna_age_sex_plot_data <- last_pub_period_dna |> 
    filter(!!sym(dataset_type_o) == dataset_choice) |>
    mutate(agg_age_groups = case_when(
        dataset_type == "CAMHS" ~ as.character(agg_age_groups),
        dataset_type == "PT" ~ as.character(agg_age_groups)),
      agg_age_groups = factor(
        agg_age_groups,
        levels = c("Under 6", "6-11", "12-15", "Over 15",
                   "Under 25", "25-39", "40-64", "65 plus")))
  
  lims = round_any(max(dna_age_sex_plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(dna_age_sex_plot_data, aes(x = agg_age_groups, y = att_rate, fill = sex_reported)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.75) +
    geom_text(aes(label = paste0(att_rate, "%")), position = position_dodge(width = 0.75),
              hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_manual(values = c("Female" = "#AF69A9", "Male" = "#3F3685")) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "Age group and sex at referral",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date),
      fill = "Sex reported") +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "right")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_sex_age_plot_last_15mth_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}



create_bar_chart_dna_simd_15mth <- function(dataset_choice){
  
  last_pub_period_dna <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb_simd.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, !!sym(simd_quintile_o)) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    mutate(firstcon_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/firstcon_apps*100,1)) |>
    filter(Attendance == 'Patient DNA',
           !is.na(!!sym(simd_quintile_o)))
  
  
  dna_simd_plot_data <- last_pub_period_dna |> 
    filter(!!sym(dataset_type_o) == dataset_choice,
           !is.na(!!sym(simd_quintile_o))) |> 
    mutate(!!sym(simd_quintile_o) := as.factor(!!sym(simd_quintile_o)),
           !!sym(simd_quintile_o) := fct_recode(!!sym(simd_quintile_o),
                                                "1 - \nMost deprived" = "1",
                                                "5 - \nLeast deprived" = "5"))
  
  lims = round_any(max(dna_age_sex_plot_data$att_rate) + 3, 2.5) # set upper limit of y axis
  
  ggplot(dna_simd_plot_data, aes(x = !!sym(simd_quintile_o), y = att_rate, fill = !!sym(simd_quintile_o))) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_text(aes(label = paste0(att_rate, "%")), hjust = 0.5, vjust = -0.4, size = 10/.pt) +
    scale_fill_discrete_phs(palette = 1) +
    scale_y_continuous(limits = c(0, lims),
                       breaks = seq(0, lims, 5),
                       labels = function(x) paste0(x,"%")) +
    labs(
      x = "SIMD Quintile",
      y = "First contact DNA rate",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.y = element_line(),
          legend.position = "none")
  
  
  ggsave(paste0(shorewise_pub_data_dir, "/appointments_firstcon/dna_simd_plot_last_15mth_", dataset_choice, ".png"),
         bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)
  
}