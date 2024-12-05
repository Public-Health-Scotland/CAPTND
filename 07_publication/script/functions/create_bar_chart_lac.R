###########################################################.
### Create bar chart to display referrals by lac status ###
###########################################################.

# Author: Bex Madden
# Date: 2024-12-04


create_bar_chart_lac <- function(){
  
# load data
  df_lac <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_lac/referrals_lac_qt_hb.parquet"))
  
# plot

df_lac_plot <- df_lac |>
  filter(ref_quarter_ending == month_end, # max(ref_quarter_ending) not working
         hb_name == "NHS Scotland") |>
  group_by(dataset_type, hb_name) |>
  mutate(total_ref = sum(count),
         perc_lac = round(count/total_ref*100, 1),
         looked_after_c_edited = factor(looked_after_c_edited, levels = c("Yes", "No", "Not known")))

lac_plot <- df_lac_plot |>
  ggplot(aes(x = fct_rev(looked_after_c_edited), y = perc_lac))+
  geom_bar(stat = "identity", fill = "#1E7F84")+
  coord_flip()+
  labs(
    y = "Percentage of total referrals",
    x = "Looked after child status",
    caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = "none")


ggsave(plot = lac_plot, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = "CAMHS_refs_by_lac.png",
       path = paste0(shorewise_pub_data_dir, "/referrals_by_lac/"))


}