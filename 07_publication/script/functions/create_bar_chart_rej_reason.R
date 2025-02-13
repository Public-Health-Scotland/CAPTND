
#########################################################.
### Create bar chart to display non acceptance reason ###
#########################################################.

# Author: Charlie Smith
# Date: 2024-07-12

#ds = "PT"

create_bar_chart_non_acceptance_reason <- function(ds = c("CAMHS", "PT")){
  
  df_reason <- read_parquet(paste0(non_acc_reason_dir, "non_acceptance_reason_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending, na.rm = TRUE) &
             !!sym(hb_name_o) == "NHS Scotland" &
             !!sym(dataset_type_o) == ds) |> 
    arrange(-prop) |> 
    mutate(count2 = format(count, big.mark = ","),
           count2 = trimws(count2),
           label = paste0(prop, "% (", count2, ")"),
           prop2 = prop / 100)
  
  vec_reasons <- unique(df_reason$ref_rej_reason_desc)#setdiff(unique(df_reason$ref_rej_reason_desc), "Other")
  ifelse(any(vec_reasons == "Other"), 
         vec_reasons <- c(vec_reasons[-which(vec_reasons == "Other")], "Other"), "")
  ifelse(any(vec_reasons == "Not recorded at board level"), 
         vec_reasons <- c(vec_reasons[-which(vec_reasons == "Not recorded at board level")], "Not recorded at board level"), "")
  
  extra_space_for_labels = 0.1
  magitude <- 10^(floor(log10(signif(max(df_reason$prop2, na.rm = TRUE), 1))))
  upper_limit <- (ceiling(max(df_reason$prop2, na.rm = TRUE) / magitude) * magitude)+ extra_space_for_labels
  
  chart <- df_reason |>
    mutate(ref_rej_reason_desc = factor(
      ref_rej_reason_desc,
      levels = vec_reasons)) |> 
    ggplot(aes(x = fct_rev(ref_rej_reason_desc), y = prop2))+
    geom_bar(stat = "identity", width = bar_width, fill = "#3F3685") +  #"#AF69A9")+ was magenta
    geom_text(aes( label = label), hjust = -0.1, size = 10/.pt)+
    scale_y_continuous(
      minor_breaks = NULL,
      limits = c(0, upper_limit),
      labels = scales::label_percent(),
      breaks = scales::breaks_extended(n = 4))+
    scale_x_discrete(labels = label_wrap(18)) +
    coord_flip()+
    labs(
      y = "Percentage of not accepted referrals",
      x = "Not accepted reasons",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.x = element_line(),
          legend.position = "none")
  
  ggsave(plot = chart, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_not_accepted_reason.png"),
         path = non_acc_reason_dir)
  
  #return(chart)
  
}
