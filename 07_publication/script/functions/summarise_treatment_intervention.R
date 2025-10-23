#########################################################.
### For publication - treatment intervention received ###
#########################################################.

# Author: Luke Taylor
# Date: 2025-10-22


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

#### SETUP #####

# load data
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

# set constants
most_recent_month_in_data <- get_lastest_month_end(df) 

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# select vars
demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")

df_treat <- df |>
  remove_borders_int_refs() |>
  filter(ref_month %in% date_range,
         !is.na(treat_1) | !is.na(treat_2) | !is.na(treat_3)) |>
  select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, treat_1,
         treat_2, treat_3, treat_start_date, header_date) |>
  mutate(treat_start_date = case_when(is.na(treat_start_date) ~ header_date,
                                       TRUE ~ treat_start_date),
         treat_month = floor_date(treat_start_date, unit = "month"),
         treat_quarter = ceiling_date(treat_month, unit = "quarter") - 1,
         treat_quarter_ending = floor_date(treat_quarter, unit = "month")) |>
  arrange(ucpn) |>
  distinct() |>
  mutate(ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                            !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                            TRUE ~ 'pending')) 


####### ALL TIME #######

#Treatment intervention 1
treat1_all <- df_treat |>
  filter(!is.na(treat_1)) |>
  group_by(dataset_type, treat_1, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_1) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_1') |>
  rename(treat_code = treat_1) |>
  arrange(dataset_type, hb_name) 

#Treatment intervention 2
treat2_all <- df_treat |>
  filter(!is.na(treat_2)) |>
  group_by(dataset_type, treat_2, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_2) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_2') |>
  rename(treat_code = treat_2) |>
  arrange(hb_name)

#Treatment intervention 3
treat3_all <- df_treat |>
  filter(!is.na(treat_3)) |>
  group_by(dataset_type, treat_3, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_3) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_3') |>
  rename(treat_code = treat_3) |>
  arrange(hb_name)

treat_all <- rbind(treat1_all, treat2_all, treat3_all) |>
  mutate(treat_code = str_remove(treat_code, "^0+"))

treat_totals_all <- treat_all |>
  group_by(treat_code, hb_name, dataset_type) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type) #|>
#save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_alltime"))


#### Get full breakdown of treatment intervention ######
# Lookup codes for treatment intervention

treat_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Treatment_update") %>% 
  mutate(Codes = as.factor(Codes)) |>
  rename(treat_name = Values,
         treat_code = Codes) |>
  select(1:2) |>
  mutate(treat_code = str_remove(treat_code, "^0+"),
         treat_name_long = paste(treat_code, treat_name, sep = " - "))

#set order of reason codes
treat_order <- treat_lookup$treat_name_long

treat_breakdown_all <- left_join(treat_all, treat_lookup, by = "treat_code") |>
  mutate(treat_name_long = factor(treat_name_long, levels = treat_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name) |>
  select(dataset_type, hb_name, treat_name_long, n, level) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/treat_alltime"))


####### QUARTERLY ########

#Treatment intervention 1
treat1_qr <- df_treat |>
  filter(!is.na(treat_1)) |>
  group_by(dataset_type, treat_1, hb_name, treat_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_1, treat_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_1') |>
  rename(treat_code = treat_1) |>
  arrange(dataset_type, hb_name) 

#Treatment intervention 2
treat2_qr  <- df_treat |>
  filter(!is.na(treat_2)) |>
  group_by(dataset_type, treat_2, hb_name, treat_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_2, treat_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_2') |>
  rename(treat_code = treat_2) |>
  arrange(hb_name)

#Treatment intervention 3
treat3_qr  <- df_treat |>
  filter(!is.na(treat_3)) |>
  group_by(dataset_type, treat_3, hb_name, treat_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_3, treat_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_3') |>
  rename(treat_code = treat_3) |>
  arrange(hb_name)

treat_qr  <- rbind(treat1_qr , treat2_qr , treat3_qr ) |>
  mutate(treat_code = str_remove(treat_code, "^0+"))

treat_totals_qr <- treat_qr  |>
  group_by(treat_code, hb_name, dataset_type, treat_quarter_ending) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, treat_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type) #|>
#save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_alltime"))


treat_breakdown_qr <- left_join(treat_qr, treat_lookup, by = "treat_code") |>
  mutate(treat_name_long = factor(treat_name_long, levels = treat_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, treat_quarter_ending) |>
  select(dataset_type, hb_name, treat_name_long, n, level) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/treat_qr"))


####### MONTHLY ########



