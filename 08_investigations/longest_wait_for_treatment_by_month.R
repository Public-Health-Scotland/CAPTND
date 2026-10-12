#########################################
####   Longest waits for treatment   #### 
#########################################

#Author: Luke Taylor
#Date: 12/10/2026

# create files for saving outputs
pat_seen_dir <- paste0(shorewise_pub_data_dir, "/patients_seen/")
dir.create(pat_seen_dir)

#load patients seen df
#df_pat_seen <- read_parquet(paste0(pat_seen_dir, "patients_seen_total_df_", month_end, ".parquet"))

#calculate longest waits
df_longest_wait <- df_pat_seen |>
  mutate(wait_end_month = floor_date(wait_end_date, unit = 'month')) |>
  group_by(wait_end_month, dataset_type, hb_name) |>
  mutate(median_wait = median(rtt_adj, na.rm = TRUE),
         max_rtt_adj = max(rtt_adj, na.rm = TRUE)) |>
  select(dataset_type, hb_name, wait_end_month, median_wait, max_rtt_adj) |>
  distinct() |>
  filter(wait_end_month >= '2021-08-01' & wait_end_month <= '2026-08-01') |>
  mutate(median_wait_in_weeks = round(median_wait/7,1),
         max_wait_in_weeks = round(max_rtt_adj/7,1)) |>
  select(-median_wait, -max_rtt_adj) |>
  pivot_longer(cols = c("median_wait_in_weeks", "max_wait_in_weeks"),
               names_to = "variable",
               values_to = "wait_in_weeks") |>
  mutate(variable = case_when(variable == 'median_wait_in_weeks' ~ 'Median wait',
                              variable == 'max_wait_in_weeks' ~ 'Max wait',
                              TRUE ~ variable))

#variable for graph
hb <- "NHS Western Isles"
ds <- "CAMHS"

#plot figures
df_longest_wait %>% filter(hb_name == hb,
                           dataset_type == ds) |>
  ggplot(aes(x = wait_end_month, y = wait_in_weeks, colour = variable, group = variable)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  scale_colour_manual(name = "Variable", values = c("#005EB8", "#76B900")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = paste0(hb, " median and maximum adjusted wait for treatment in weeks"),
       x = "Month", y = "Length of wait for treatment (weeks)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  



### EXCLUDING DIGITAL THERAPIES ###
#calculate longest waits and median waits
df_longest_wait_ex_digi <- df_pat_seen |>
  filter(is.na(act_code_sent_date)) |>
  mutate(wait_end_month = floor_date(wait_end_date, unit = 'month')) |>
  group_by(wait_end_month, dataset_type, hb_name) |>
  mutate(median_wait = median(rtt_adj, na.rm = TRUE),
         max_rtt_adj = max(rtt_adj, na.rm = TRUE)) |>
  select(dataset_type, hb_name, wait_end_month, median_wait, max_rtt_adj) |>
  distinct() |>
  filter(wait_end_month >= '2021-08-01' & wait_end_month <= '2026-08-01') |>
  mutate(median_wait_in_weeks = round(median_wait/7,1),
         max_wait_in_weeks = round(max_rtt_adj/7,1)) |>
  select(-median_wait, -max_rtt_adj) |>
  pivot_longer(cols = c("median_wait_in_weeks", "max_wait_in_weeks"),
               names_to = "variable",
               values_to = "wait_in_weeks") |>
  mutate(variable = case_when(variable == 'median_wait_in_weeks' ~ 'Median wait',
                              variable == 'max_wait_in_weeks' ~ 'Max wait',
                              TRUE ~ variable))

#variable for graph
hb <- "NHS Orkney"
ds <- "PT"

#plot figures
df_longest_wait_ex_digi %>% filter(hb_name == hb,
                                   dataset_type == ds) |>
  ggplot(aes(x = wait_end_month, y = wait_in_weeks, colour = variable, group = variable)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  scale_colour_manual(name = "Variable", values = c("#005EB8", "#76B900")) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = paste0(hb, " median and maximum adjusted wait for treatment in weeks (excluding digital therapies)"),
       x = "Month", y = "Length of wait for treatment (weeks)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  



### COUNT OF DIGITAL THERAPIES ONLY ###
#calculate longest waits and median waits
df_digi_treat_starts <- df_pat_seen |>
  filter(!is.na(act_code_sent_date)) |>
  mutate(wait_end_month = floor_date(wait_end_date, unit = 'month')) |>
  group_by(wait_end_month, dataset_type, hb_name) |>
  summarise(count = n()) 

#variable for graph
hb <- "NHS Forth Valley"
ds <- "PT"

#plot figures
df_digi_treat_starts %>% filter(hb_name == hb,
                                dataset_type == ds) |>
  ggplot(aes(x = wait_end_month, y = count)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 0.8, na.rm = TRUE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = paste0(hb, " patients starting digital therapy by month"),
       x = "Month", y = "Number of patients") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  


