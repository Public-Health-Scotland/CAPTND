###############################################################.
### Create bar chart to display referrals by veteran status ###
###############################################################.

# Author: Bex Madden
# Date: 2024-12-04

create_bar_chart_ppmh <- function(){
  
#load data
  df_ppmh <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/referrals_ppmh_qt_hb_sex.parquet"))
  
# plot

df_plot <- df_ppmh |>
  filter(ref_quarter_ending == month_end,
         hb_name == "NHS Scotland",
         sex_reported == "Female") |>
  group_by(dataset_type, hb_name) |>
  mutate(total_ref = sum(count),
         perc_reason = round(count/total_ref*100, 1),
         preg_perinatal = factor(preg_perinatal, levels = c("Currently pregnant", "Post-natal (within 12m)", "Not pregnant", "Not applicable", "Not known")))

plot <- df_plot |>
  ggplot(aes(x = fct_rev(preg_perinatal), y = perc_reason))+
  geom_bar(stat = "identity", fill = "#3F3685")+
  scale_x_discrete(labels = label_wrap(20)) +
  coord_flip()+
  labs(
    y = "Percentage of total referrals",
    x = "Pregnancy/perinatal status",
    caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = "none")


ggsave(plot = plot, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = "PT_refs_by_ppmh.png",
       path = paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/"))

}

