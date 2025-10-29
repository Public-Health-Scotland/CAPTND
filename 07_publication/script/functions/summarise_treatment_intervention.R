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

create_treatment_df <- function(treat_type = c('treat_1', 'treat_2', 'treat_3')){
  
  df_treat <- df |>
    remove_borders_int_refs() |>
    filter(header_date %in% date_range, #using header date rather than ref month
           !is.na(.data[[treat_type]])) |>
    select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, all_of(treat_type), treat_start_date, header_date) |>
    arrange(ucpn, .data[[treat_type]], treat_start_date, header_date) |>
    group_by(!!!syms(data_keys), .data[[treat_type]]) |>
    slice_head(n = 1) |>
    ungroup() |>
    mutate(treat_start_date = case_when(is.na(treat_start_date) ~ header_date,
                                        TRUE ~ treat_start_date), #use header date if treat_start_date is missing
           treat_month = floor_date(treat_start_date, unit = "month"),
           treat_quarter = ceiling_date(treat_month, unit = "quarter") - 1,
           treat_quarter_ending = floor_date(treat_quarter, unit = "month"))
  
}

df_treat_1 <- create_treatment_df('treat_1')
df_treat_2 <- create_treatment_df('treat_2')
df_treat_3 <- create_treatment_df('treat_3')


####### ALL TIME #######

#Treatment intervention 1
treat1_all <- df_treat_1 |>
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
treat2_all <- df_treat_2 |>
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
  arrange(dataset_type, hb_name)

#Treatment intervention 3
treat3_all <- df_treat_3 |>
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
  arrange(dataset_type, hb_name)

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
treat1_qr <- df_treat_1 |>
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
  arrange(dataset_type, hb_name, treat_quarter_ending) 

#Treatment intervention 2
treat2_qr  <- df_treat_2 |>
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
  arrange(dataset_type, hb_name, treat_quarter_ending)

#Treatment intervention 3
treat3_qr  <- df_treat_3 |>
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
  arrange(dataset_type, hb_name, treat_quarter_ending)

treat_qr  <- rbind(treat1_qr , treat2_qr , treat3_qr ) |>
  mutate(treat_code = str_remove(treat_code, "^0+"))

treat_totals_qr <- treat_qr  |>
  group_by(treat_code, hb_name, dataset_type, treat_quarter_ending) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, treat_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type, treat_quarter_ending) #|>
#save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_alltime"))


treat_breakdown_qr <- left_join(treat_qr, treat_lookup, by = "treat_code") |>
  mutate(treat_name_long = factor(treat_name_long, levels = treat_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, treat_quarter_ending) |>
  select(dataset_type, hb_name, treat_quarter_ending, treat_name_long, n, level) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/treat_qr"))


####### MONTHLY ########

#Treatment intervention 1
treat1_mth <- df_treat_1 |>
  filter(!is.na(treat_1)) |>
  group_by(dataset_type, treat_1, hb_name, treat_month) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_1, treat_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_1') |>
  rename(treat_code = treat_1) |>
  arrange(dataset_type, hb_name, treat_month) 

#Treatment intervention 2
treat2_mth  <- df_treat_2 |>
  filter(!is.na(treat_2)) |>
  group_by(dataset_type, treat_2, hb_name, treat_month) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_2, treat_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_2') |>
  rename(treat_code = treat_2) |>
  arrange(dataset_type, hb_name, treat_month)

#Treatment intervention 3
treat3_mth  <- df_treat_3 |>
  filter(!is.na(treat_3)) |>
  group_by(dataset_type, treat_3, hb_name, treat_month) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_3, treat_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'treat_3') |>
  rename(treat_code = treat_3) |>
  arrange(dataset_type, hb_name, treat_month)

treat_mth  <- rbind(treat1_mth, treat2_mth, treat3_mth) |>
  mutate(treat_code = str_remove(treat_code, "^0+"))

treat_totals_mth <- treat_mth  |>
  group_by(treat_code, hb_name, dataset_type, treat_month) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, treat_month) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type, treat_month)


treat_breakdown_mth <- left_join(treat_mth, treat_lookup, by = "treat_code") |>
  mutate(treat_name_long = factor(treat_name_long, levels = treat_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, treat_month) |>
  select(dataset_type, hb_name, treat_month, treat_name_long, n, level) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/treat_mth"))

