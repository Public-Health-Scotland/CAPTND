###############################################################.
### Create bar chart to display referrals by veteran status ###
###############################################################.

# Author: Luke Taylor + Bex Madden
# Date: 2024-12-04


create_bar_chart_veteran <- function(){

# load data
  df_vet <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_vet_status/referrals_vets_qr_hb.parquet"))

# plot
df_vets <- df_vet |> #latest quarter NHSScotland
  filter(!!sym(hb_name_o) == 'NHS Scotland',
         ref_quarter_ending == max(ref_quarter_ending)) |>
  
  mutate(vet_label = case_when(vet_label == "Data missing" ~ "Not known", TRUE ~ vet_label)) |> 
  group_by(!!sym(hb_name_o), vet_label) |>
  summarise(count = sum(count), .groups = "drop") |> 
  
  mutate(tot = sum(count),
         prop = round(count/tot*100, 1),
         vet_label = factor(vet_label, levels = c("Yes", "No", "Not known")))
         # count2 = format(count, big.mark = ","),
         # count2 = trimws(count2),
         # label = paste0(prop, "% (", count2, ")"),
         # prop2 = prop / 100)

# extra_space_for_labels = 0.1
# magitude <- 10^(floor(log10(signif(max(df_vets$prop2, na.rm = TRUE), 1))))
# upper_limit <- (ceiling(max(df_vets$prop2, na.rm = TRUE) / magitude) * magitude)+ extra_space_for_labels

chart <- df_vets |>
  ggplot(aes(x = fct_rev(vet_label), y = prop))+
  geom_bar(stat = "identity", width = bar_width, fill = "#655E9D")+
  #geom_text(aes( label = label), hjust = -0.1, size = 10/.pt)+
  # scale_y_continuous( ## ?? use scale_y_sqrt(limits = c(0,100), breaks = seq(0,100, by=10)) +
  #   minor_breaks = NULL,
  #   limits = c(0, upper_limit),
  #   labels = scales::label_percent(),
  #   breaks = scales::breaks_extended(n = 5))+
  
  scale_x_discrete(labels = label_wrap(20)) +
  coord_flip()+
  labs(
    y = "Percentage of total referrals",
    x = "Veteran Status",
    caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
  theme_captnd() +
  theme(panel.grid.major.x = element_line(),
        legend.position = "none")


ggsave(plot = chart, device = "png", bg = "white", 
       width = chart_width, height = chart_height, units = "cm", dpi = 300,
       filename = "PT_refs_by_veteran.png",
       path = paste0(shorewise_pub_data_dir, "/referrals_by_vet_status/"))


}

