################################################################.
### Combine adjusted and unadjusted patients waiting figures ###
################################################################.

# Author: Luke Taylor
# Date: 2024-09-24

dir.create(pat_waits_dir)
measure_label <- "patients_wait_"

#Load adjusted waiting times, and apply wait weeks
df_adj_waits <- read_parquet(paste0(pat_waits_dir, "patients_waiting_adj_", month_end, ".parquet")) |>
  ungroup() |>
  select(!!!syms(data_keys), sub_month_end, wait_days_adj = monthly_wait_adj) |>
  mutate(wait_wks_adj = round(wait_days_adj/7, 1),
         wait_group_adj = case_when(
           wait_wks_adj >= 0 & wait_wks_adj <= 18 ~ "wait_0_to_18_weeks",
           wait_wks_adj > 18 & wait_wks_adj <= 35 ~ "wait_19_to_35_weeks",
           wait_wks_adj > 35 & wait_wks_adj <= 52 ~ "wait_36_to_52_weeks",
           wait_wks_adj > 52 ~ "over_52_weeks",
           TRUE ~ NA_character_),
         wait_group_adj = factor(wait_group_adj, levels = c("wait_0_to_18_weeks", "wait_19_to_35_weeks",
                                                                "wait_36_to_52_weeks", "over_52_weeks")))


df_waits_complete <- read_parquet(paste0(pat_waits_dir, measure_label, "complete_wl_", month_end, ".parquet")) |>
  filter(hb_name == 'NHS Dumfries and Galloway') |>
  left_join(df_adj_waits, by = c('dataset_type', 'hb_name', 'ucpn', 'patient_id', 'sub_month_end')) |>
  mutate(wait_days_adj = case_when(is.na(wait_days_adj) & !is.na(ref_rej_date) ~ wait_days_unadj,
                                   TRUE ~ wait_days_adj),
         wait_wks_adj = case_when(is.na(wait_wks_adj) & !is.na(ref_rej_date) ~ wait_wks_unadj,
                                  TRUE ~ wait_wks_adj),
         wait_group_adj = case_when(is.na(wait_group_adj) & !is.na(ref_rej_date) ~ wait_group_unadj,
                                    TRUE ~ wait_group_adj))
  
# by month ----------------------------------------------------------------

sub_month_start_o <- "sub_month_start"
wait_group_unadj_o <- "wait_group_unadj"
wait_group_adj_o <- "wait_group_adj"

# by hb and month
df_month_hb <- df_waits_complete |> 
  select(!!!syms(data_keys), sex_reported, age_group, simd2020_quintile, sub_month_start, 
         wait_days_unadj, wait_wks_unadj, wait_group_unadj, wait_days_adj, wait_wks_adj, wait_group_adj) |>
  filter(sub_month_start %in% date_range)

#unadjusted
df_month_unadj_hb <- df_month_hb |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o, wait_group_unadj_o))) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  group_by(!!!syms(c(dataset_type_o, sub_month_start_o, wait_group_unadj_o))) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
  filter(!(hb_name == "NHS 24")) |>
  rename(wait_group = wait_group_unadj) |>
  save_as_parquet(path = paste0(pat_waits_dir, measure_label, "unadj_month_hb")) |> 
  append_quarter_ending(date_col = "sub_month_start") |> 
  group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o))) |> 
  filter(!!sym(sub_month_start_o) == max(!!sym(sub_month_start_o))) |> # need last value per quarter only
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "wait_group")) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(pat_waits_dir, measure_label, "unadj_quarter_hb"))


#adjusted
df_month_adj_hb <- df_month_hb |>
  group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o, wait_group_adj_o))) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  group_by(!!!syms(c(dataset_type_o, sub_month_start_o, wait_group_adj_o))) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!hb_name_o, ~"NHS Scotland"),
                      .groups = "drop"))|> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o))|> 
  right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
  filter(!(hb_name == "NHS 24")) |>
  rename(wait_group = wait_group_adj) |>
  save_as_parquet(path = paste0(pat_waits_dir, measure_label, "adj_month_hb")) |> 
  append_quarter_ending(date_col = "sub_month_start") |> 
  group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o))) |> 
  filter(!!sym(sub_month_start_o) == max(!!sym(sub_month_start_o))) |> # need last value per quarter only
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "wait_group")) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(pat_waits_dir, measure_label, "adj_quarter_hb"))

#combine datasets

wait_group_vector <- c("wait_0_to_18_weeks", 
                       "wait_19_to_35_weeks",
                       "wait_36_to_52_weeks",
                       "over_52_weeks")

df_wait_group <- cross_join(as.data.frame(vec_dataset_type),
                            as.data.frame(hb_vector)) |>
  cross_join(as.data.frame(wait_group_vector)) |>
  cross_join(df_quarts) |>
  distinct() |>
  rename(dataset_type = vec_dataset_type,
         wait_group = wait_group_vector,
         hb_name = hb_vector) |>
  mutate(wait_group = factor(wait_group, levels = wait_group_vector))


df_mth_wait_complete <- df_month_unadj_hb |>
  rename(unadjusted = count) |>
  right_join(df_wait_group, by = c("quarter_ending", "dataset_type", "hb_name", "wait_group")) |>
  left_join(df_month_adj_hb, by = c('quarter_ending', 'dataset_type', 'hb_name', 'wait_group')) |>
  rename(adjusted = count) |>
  mutate(unadjusted = case_when(is.na(unadjusted) ~ 0,
                                TRUE ~ unadjusted),
         adjusted = case_when(is.na(adjusted) ~ 0,
                              TRUE ~ adjusted)) |>
  # pivot_longer(cols = c('unadjusted', 'adjusted'),
  #              names_to = 'adjustment_status',
  #              values_to = 'rank') |>
  group_by(quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o)) |>
  mutate(adj_tot = sum(adjusted),
         unadj_tot = sum(unadjusted)) 





