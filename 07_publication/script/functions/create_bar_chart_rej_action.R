
#########################################################.
### Create bar chart to display non acceptance action ###
#########################################################.

# Author: Charlie Smith
# Date: 2024-07-15
#ds = "CAMHS"

create_bar_chart_non_acceptance_action <- function(ds = c("CAMHS", "PT")){
  
  df_action <- read_parquet(paste0(non_acc_action_dir, "non_acceptance_action_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending == max(quarter_ending, na.rm = TRUE) &
             !!sym(hb_name_o) == "NHS Scotland" &
             !!sym(dataset_type_o) == ds) |> 
    arrange(-prop) |> 
    mutate(count2 = format(count, big.mark = ","),
           count2 = trimws(count2),
           label = paste0(prop, "% (", count2, ")"),
           prop2 = prop / 100)
  
  #vec_reasons <- setdiff(unique(df_action$ref_rej_act_desc), "Other")
  vec_reasons <- unique(df_action$ref_rej_act_desc)
  ifelse(any(vec_reasons == "Not recorded at board level"), 
         vec_reasons <- c(vec_reasons[-which(vec_reasons == "Not recorded at board level")], "Not recorded at board level"), "")
  
  extra_space_for_labels = 0.1
  magitude <- 10^(floor(log10(signif(max(df_action$prop2, na.rm = TRUE), 1))))
  upper_limit <- (ceiling(max(df_action$prop2, na.rm = TRUE) / magitude) * magitude)+ extra_space_for_labels
  
  chart <- df_action |>
    mutate(ref_rej_act_desc = factor(
      ref_rej_act_desc,
      levels = vec_reasons)) |> 
    ggplot(aes(x = fct_rev(ref_rej_act_desc), y = prop2))+
    geom_bar(stat = "identity", width = bar_width, fill = "#3F3685")+
    geom_text(aes( label = label), hjust = -0.1, size = 10/.pt)+
    scale_y_continuous(
      minor_breaks = NULL,
      limits = c(0, upper_limit),
      labels = scales::label_percent(),
      breaks = scales::breaks_extended(n = 5))+
    scale_x_discrete(labels = scales::label_wrap(20)) +
    coord_flip()+
    labs(
      y = "Percentage of not accepted referrals",
      x = "Not accepted actions",
      caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
    theme_captnd() +
    theme(panel.grid.major.x = element_line(),
          legend.position = "none")
  
  chart_width <- 22
  
  ggsave(plot = chart, device = "png", bg = "white", 
         width = chart_width, height = chart_height, units = "cm", dpi = 300,
         filename = paste0(ds, "_not_accepted_action.png"),
         path = non_acc_action_dir)
  
  #return(chart)
  
}
