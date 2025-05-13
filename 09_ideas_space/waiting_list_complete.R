################################################################.
### Combine adjusted and unadjusted patients waiting figures ###
################################################################.

# Author: Luke Taylor
# Date: 2024-09-24

#Load adjusted waiting times, and apply wait weeks
df_adj_waits <- read_parquet(paste0(pat_waits_dir, "patients_waiting_adj_", month_end, ".parquet")) |>
  ungroup() |>
  select(!!!syms(data_keys), sub_month_end, monthly_wait_adj) |>
  mutate(wait_wks_adj = round(monthly_wait_adj/7, 1),
         wait_group_adj = case_when(
           wait_wks_adj >= 0 & wait_wks_adj <= 18 ~ "wait_0_to_18_weeks",
           wait_wks_adj > 18 & wait_wks_adj <= 35 ~ "wait_19_to_35_weeks",
           wait_wks_adj > 35 & wait_wks_adj <= 52 ~ "wait_36_to_52_weeks",
           wait_wks_adj > 52 ~ "over_52_weeks",
           TRUE ~ NA_character_),
         wait_group_adj = factor(wait_group_adj, levels = c("wait_0_to_18_weeks", "wait_19_to_35_weeks",
                                                                "wait_36_to_52_weeks", "over_52_weeks")))


df_waits_complete <- read_parquet(paste0(pat_waits_dir, measure_label, "complete_wl_", month_end, ".parquet")) |>
  filter(hb_name == 'NHS Western Isles') |>
  left_join(df_adj_waits, by = c('dataset_type', 'hb_name', 'ucpn', 'patient_id', 'sub_month_end'))
  
