
#########################################
## Create Referral Source Pareto Chart ##
#########################################

# Author: Luke Taylor
# Date: 2025-01-14

create_pareto_chart_ref_reason <- function(ds = c("CAMHS", "PT")){

ref_source_pub <- read_parquet(paste0(ref_source_dir, "ref_source_quarter_hb.parquet")) |>
  filter(!!sym(dataset_type_o) == dataset_choice) |>
  filter(!!sym(hb_name_o) == 'NHS Scotland' &
           quarter_ending == month_end) |>
  mutate(ref_source_name = case_when(ref_source_name == 'Self referral (includes self, relations, friends and carers)' ~ 'Self referral',
                                     TRUE ~ as.character(ref_source_name))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  arrange(desc(count), .by_group = TRUE) |>
  mutate(rank = row_number())

#all other sources
agg_df <- ref_source_pub |>
  filter(rank > 5) |>
  group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
  summarise(count = sum(count)) |>
  mutate(ref_source_name = 'All other referral sources',
         rank = 6) |>
  select(quarter_ending, dataset_type, hb_name, ref_source_name, count, rank)

#top 5 referral sources
top5_df <- ref_source_pub |>
  filter(rank <= 5) |>
  select(quarter_ending, dataset_type, hb_name, ref_source_name, count, rank)

#create final df for output
df_ref_source <- rbind(top5_df, agg_df) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |>
  change_nhsscotland_label() |>
  filter(!!sym(dataset_type_o) == dataset_choice) |>
  arrange(desc(count))

df_ref_source <- df_ref_source |>
  mutate(cum_sum = cumsum(count),
         ncol = cum_sum > 0.8*max(cum_sum)) |>
  mutate(ref_source_name = factor(ref_source_name, levels = ref_source_name[order(1/count)])) |>
  filter(count > 100) 


lims = round_any(max(ref_source_pub$total), 10000) # set upper limit of y axis


source_chart <- df_ref_source %>% 
  ggplot(aes(x = ref_source_name)) +
  geom_col(aes(y = count, fill = dataset_type)) +
  geom_point(aes(y = cum_sum)) +
  geom_path(aes(y = cum_sum), group = 1) +
  geom_hline(aes(yintercept = 0.8*max(cum_sum)), linetype = "dashed", size = 0.5) +
  annotate("text", x = 6.4, y = 16000, label = "80%", size = 4) +
  scale_fill_discrete_phs() +
  theme_captnd() +
  scale_y_continuous(limits = c(0, lims),
                     breaks = seq(0, lims, 2500),
                     labels = scales::comma) +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "Referral Source",
       y = "Number of Referrals",
       caption = paste0("CAPTND extract, ", data_analysis_latest_date))


ggsave(paste0(ref_source_dir, "ref_source_pareto_", dataset_choice, ".png"),
       bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)

}
