##################################################################.
### Create bar chart to display referrals by protection status ###
##################################################################.

# Author: Luke Taylor + Bex Madden
# Date: 2024-12-04

create_bar_chart_prot <- function(ds = c("CAMHS", "PT")) {
  
# load data

if(ds == "CAMHS"){
  df_prot <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/referrals_prot_child_qr_hb.parquet"))
}else{
  df_prot <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/referrals_prot_adult_qr_hb.parquet"))
}

age_label <- ifelse(ds == "CAMHS", "Child", "Adult")


#plot
df_ps <- df_prot |> #can show quarter or period total?
  filter(!!sym(hb_name_o) == 'NHS Scotland') |>
  
  mutate(prot_label = case_when(prot_label == "Data missing" ~ "Not known", TRUE ~ prot_label)) |> 
  group_by(!!sym(hb_name_o), prot_label) |>
  summarise(count = sum(count), .groups = "drop") |> 
  
  group_by(!!sym(hb_name_o)) |>
  mutate(tot = sum(count),
         prop = round(count/tot*100,1),
         prot_label = factor(prot_label, levels = c("Yes", "No", "Not known")))
#          count2 = format(count, big.mark = ","),
#          count2 = trimws(count2),
#          label = paste0(prop, "% (", count2, ")"),
#          prop2 = prop / 100)
# 
# extra_space_for_labels = 0.1
# magitude <- 10^(floor(log10(signif(max(df_aps$prop2, na.rm = TRUE), 1))))
# upper_limit <- (ceiling(max(df_aps$prop2, na.rm = TRUE) / magitude) * magitude)+ extra_space_for_labels

chart <- df_ps |>
  ggplot(aes(x = fct_rev(prot_label), y = prop))+
  geom_bar(stat = "identity", width = bar_width, fill = "#655E9D")+
 # geom_text(aes( label = label), hjust = -0.1, size = 10/.pt)+
  # scale_y_continuous(
  #   minor_breaks = NULL,
  #   limits = c(0, upper_limit),
  #   labels = scales::label_percent(),
  #   breaks = scales::breaks_extended(n = 5))+
  scale_x_discrete(labels = label_wrap(20)) +
  coord_flip()+
  labs(
    y = "Percentage of total referrals",
    x = paste0(age_label, " protection status"),
    caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = "none")


ggsave(plot = chart, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = paste0(ds, "_refs_by_prot_status.png"),
       path = paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/"))

}
