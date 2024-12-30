
month_end <- "2024-06-01"

source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")


move_to_last <- function(df, n) df[c(setdiff(seq_len(nrow(df)), n), n), ]


ref_source_qr_hb <- read_parquet(paste0(ref_source_dir, "ref_source_quarter_hb.parquet"))

ref_source_pub <- ref_source_qr_hb |>
  filter(!!sym(dataset_type_o) == dataset_choice) |>
  filter(!!sym(hb_name_o) == 'NHS Scotland' &
           quarter_ending == month_end) |>
  mutate(ref_source_name = case_when(is.na(ref_source_name) ~ 'Data Missing',
                                     ref_source_name == 'Self referral (includes self, relations, friends and carers)' ~ 'Self referral',
                                     TRUE ~ as.character(ref_source_name))) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  arrange(desc(count), .by_group = TRUE)

ref_source_pub <- move_to_last(ref_source_pub, 10)

ref_source_pub <- ref_source_pub |>
  mutate(cum_sum = cumsum(count),
         ncol = cum_sum > 0.8*max(cum_sum)) |>
  mutate(ref_source_name = factor(ref_source_name, levels = ref_source_name[order(1/count)])) |>
  filter(count > 100 &
           ref_source_name != 'Data Missing') 


lims = round_any(max(ref_source_pub$total), 10000) # set upper limit of y axis



source_chart <- ref_source_pub %>% 
  ggplot(aes(x = ref_source_name)) +
  geom_col(aes(y = count, fill = dataset_type)) +
  geom_point(aes(y = cum_sum)) +
  geom_path(aes(y = cum_sum), group = 1) +
  geom_hline(aes(yintercept = 0.8*max(cum_sum)), linetype = "dashed", size = 0.5) +
  geom_text(aes(y = 0.8*max(cum_sum), x = Inf), label = "80%", vjust = -0.5, hjust = 1,size = 4) +
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

chart_height <- 14
chart_width <- 20

ggsave(paste0(ref_source_dir, "ref_source_pareto_", dataset_choice, ".png"),
       bg = "white", width = chart_width, height = chart_height, units = "cm", dpi = 300)

#CAMHS
#Figure x shows the source of referrals for people referred to x services across Scotland for 
#the quarter ending June 2024. The bars show the number of referrals received by each source
#and the curve shows the cumulative percentage of referrals. Only referral sources for which
#more than 100 referrals were received during the quarter have been displayed in the chart.
#The chart shows us that GP is the most frequently recorded referral source with x referrals,
#and that around 80% of referrals come from the following source; GP, Consultant and Teacher.
#There were referrals for which the referral source was 'Unknown' and a further x referrals
#for which the data was missing.

#PT
#Figure x shows the source of referrals for people referred to x services across Scotland for 
#the quarter ending June 2024. The bars show the number of referrals received by each source
#and the curve shows the cumulative percentage of referrals. Only referral sources for which
#more than 100 referrals were received during the quarter have been displayed in the chart.
#The chart shows us that GP is the most frequently recorded referral source with x referrals,
#and that around 80% of referrals come from the following source; GP and Consultant.
#There were 494 (2.6%) referrals for which the referral source was 'Unknown' and a further x
#referrals for which the data was missing.

