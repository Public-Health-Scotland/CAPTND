
########################.
### Create DQ Charts ###
########################.

# Author: Charlie Smith
# Date: 2024-05-08

# load data

df_charts <- read_parquet(paste0(data_quality_report_dir, "/captnd_dq_clean_latest.parquet")) |> 
  add_proportion_groups()

chart_known <- create_heatmap_known(df = df_charts, chart_value = "known")
#chart_missing <- ggplot_heatmap_missing(df = df_charts, chart_value = "Missing")
#chart_not_known <- ggplot_heatmap_not_known(df = df_charts, chart_value = "Not known")
#chart_invalid <- ggplot_heatmap_invalid(df = df_charts, chart_value = "Invalid")

# save plots 
vec_value_lower <- tolower(vec_value) %>% str_replace(., " ", "_") # concat values for filenames
for(i in 1:length(vec_value_lower)){
  ggsave(filename = paste0(dir_output, "/", vec_value_lower[i],"_update.png"), 
         plot = get(paste0("chart_", vec_value_lower[i])), device = "png", 
         width = 17, height = 10, units = "in", dpi = 300)
  
}

