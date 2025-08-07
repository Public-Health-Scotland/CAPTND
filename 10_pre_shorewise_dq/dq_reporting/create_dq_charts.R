
########################.
### Create DQ Charts ###
########################.

# Author: Charlie Smith
# Date: 2024-05-08

# load data

df_charts <- read_parquet(paste0(data_quality_report_dir, "/captnd_dq_clean_latest.parquet")) |> 
  #filter(dataset_type == "CAMHS" & hb_name == "NHS24" & variable == "unav_date_end") |> 
  add_proportion_groups() |> 
  append_variable_categories() |> 
  append_nhsscotland_label_factor() |>
  update_nas_and_zeros()                                                        #changes zeros to "-", and NAs to ". ."

chart_known <- create_heatmap_known(df = df_charts, chart_value = "known")
chart_missing <- create_heatmap_missing(df = df_charts, chart_value = "missing")
chart_not_known <- create_heatmap_not_known(df = df_charts, chart_value = "not known")
chart_invalid <- create_heatmap_invalid(df = df_charts, chart_value = "invalid")

# save plots 
vec_value_lower <- tolower(vec_value) %>% str_replace(., " ", "_") # concat values for filenames

for(i in 1:length(vec_value_lower)){
  ggsave(filename = paste0(data_quality_report_dir, "/", vec_value_lower[i],"_update.png"), 
         plot = get(paste0("chart_", vec_value_lower[i])), device = "png", 
         width = 17, height = 10, units = "in", dpi = 300)
  
}

