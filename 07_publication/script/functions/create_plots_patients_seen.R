##############################################.
#### Create patients seen plots - for mmi ####.
##############################################.

# Author: Bex Madden
# Date: 2024-09-19


create_plots_patients_seen <- function(dataset_choice){
  
  # adjusted waiting times
  pat_seen_summ_adj <- read_parquet(paste0(pat_seen_dir, "pat_seen_adj_wait_grp_mth.parquet"))
  
  dates <- pat_seen_summ_adj |> # make date labels for quarters
    select(first_treat_month) |>
    unique() |>
    pull()
  
  # line plot showing % meeting 18 week standard by month
  adj_line_plot <- pat_seen_summ_adj |> 
    filter(first_treat_month >=  ymd(month_start),
           adj_rtt_group == "0 to 18 weeks",
           hb_name == "NHS Scotland",
           dataset_type == dataset_choice) |> 
    ggplot(aes(x = first_treat_month, y = perc)) +
    geom_point() +
    geom_line() +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    ylim(0, 100) +# shape similar to agg
    labs(
      x = "\nTreatment Start Month",
      y = "% patients seen <18 weeks",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  ggsave(paste0(pat_seen_dir, "pat_seen_adj_trend_", dataset_choice, ".png"),
         bg = "white", width = 20, height = 13, units = "cm", dpi = 300)
  
  # bar plot showing count in each wait group
  adj_bar_plot <- pat_seen_summ_adj |> 
    filter(first_treat_month >=  ymd(month_start),
           hb_name == "NHS Scotland",
           dataset_type == dataset_choice) |> 
    ggplot(aes(x = first_treat_month, y = n, fill = fct_rev(as_factor(adj_rtt_group)))) +
    geom_bar(position = "stack", stat="identity") + # shape similar but number quite different to agg
    scale_fill_discrete_phs() +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    labs(
      x = "\nTreatment Start Month",
      y = "N patients by wait group",
      fill = "Wait group",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  ggsave(paste0(pat_seen_dir, "pat_seen_adj_bar_", dataset_choice, ".png"),
         bg = "white", width = 20, height = 13, units = "cm", dpi = 300) #width = chart_width, height = chart_height
  
  
  # unadjusted waiting times
  pat_seen_summ_unadj <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_mth.parquet"))
  
  # line plot showing % meeting 18 week standard by month
  unadj_line_plot <- pat_seen_summ_unadj |> 
    filter(first_treat_month >=  ymd(month_start),
           unadj_rtt_group == "0 to 18 weeks",
           hb_name == "NHS Scotland",
           dataset_type == dataset_choice) |> 
    ggplot(aes(x = first_treat_month, y = perc)) +
    geom_point() +
    geom_line() +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    ylim(0, 100) +# shape similar to agg
    labs(
      x = "\nTreatment Start Month",
      y = "% patients seen <18 weeks",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  ggsave(paste0(pat_seen_dir, "pat_seen_unadj_trend_", dataset_choice, ".png"),
         bg = "white", width = 20, height = 13, units = "cm", dpi = 300)
  
  # bar plot showing count in each wait group
  unadj_bar_plot <- pat_seen_summ_unadj |> 
    filter(first_treat_month >=  ymd(month_start),
           hb_name == "NHS Scotland",
           dataset_type == dataset_choice) |> 
    ggplot(aes(x = first_treat_month, y = n, fill = fct_rev(as_factor(unadj_rtt_group)))) +
    geom_bar(position="stack", stat="identity") +
    scale_fill_discrete_phs() +
    scale_x_date(labels = format(dates, "%b-%y"), breaks = dates) +
    labs(
      x = "\nTreatment Start Month",
      y = "N patients by wait group",
      fill = "Wait group",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  ggsave(paste0(pat_seen_dir, "pat_seen_unadj_bar_", dataset_choice, ".png"),
         bg = "white", width = 20, height = 13, units = "cm", dpi = 300)
  
}

