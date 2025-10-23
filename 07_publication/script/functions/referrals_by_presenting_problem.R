#########################################################.
### For publication - referrals by presenting problem ###
#########################################################.

# Author: Luke Taylor
# Date: 2025-10-22


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

#### SETUP #####

# load data
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(ref_month = floor_date(ref_rec_date, unit = "month"),
         ref_quarter = ceiling_date(ref_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) 

# set constants
most_recent_month_in_data <- get_lastest_month_end(df) 

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# select vars
demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")

df_refs <- df |>
  remove_borders_int_refs() |>
  filter(ref_month %in% date_range) |>
  select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, ref_quarter_ending, presenting_prob_1,
         presenting_prob_2, presenting_prob_3, ref_month, app_date, att_status) |>
  arrange(ucpn, app_date) |>
  mutate(presenting_prob_1 = as.numeric(presenting_prob_1),
         presenting_prob_2 = as.numeric(presenting_prob_2),
         presenting_prob_3 = as.numeric(presenting_prob_3)) |>
  group_by(!!!syms(data_keys)) |>
  fill(presenting_prob_1, .direction = 'updown') |>
  fill(presenting_prob_2, .direction = 'updown') |>
  fill(presenting_prob_3, .direction = 'updown') |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                            !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                            TRUE ~ 'pending')) 



####### ALL TIME #######

#Presenting problem 1
pp1_all <- df_refs |>
  group_by(dataset_type, presenting_prob_1, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_1) |
      presenting_prob_1 == "99" |
      presenting_prob_1 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_1, presenting_prob) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_1') |>
  rename(presenting_prob_code = presenting_prob_1) |>
  arrange(hb_name) 

#Presenting problem 2
pp2_all <- df_refs |>
  group_by(dataset_type, presenting_prob_2, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_2) |
      presenting_prob_2 == "99" |
      presenting_prob_2 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_2, presenting_prob) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_2') |>
  rename(presenting_prob_code = presenting_prob_2) |>
  arrange(hb_name)

#Presenting problem 3
pp3_all <- df_refs |>
  group_by(dataset_type, presenting_prob_3, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_3) |
      presenting_prob_3 == "99" |
      presenting_prob_3 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_3, presenting_prob) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_3') |>
  rename(presenting_prob_code = presenting_prob_3) |>
  arrange(hb_name)

pp_all <- rbind(pp1_all, pp2_all, pp3_all)

pp_totals_all <- pp_all |>
  group_by(presenting_prob, hb_name, dataset_type) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = presenting_prob, values_from = c("totals", "perc_refs")) |>
  mutate(across(all_refs:totals_valid_reason, ~prettyNum(., big.mark = ","))) |>
  arrange(hb_name, dataset_type) #|>
  #save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_alltime"))


#### Get full breakdown of presenting problem ######
# Lookup codes for presenting problem

pp_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Presenting_Problem") %>% 
  select(-Further.Information) |>
  mutate(Codes = as.factor(Codes)) |>
  rename(present_prob_name = Values,
         presenting_prob_code = Codes)

#set order of reason codes
pp_order <- pp_lookup$present_prob

pp_breakdown_all <- left_join(pp_all, pp_lookup, by = "presenting_prob_code") |> 
  mutate(present_prob_name = factor(present_prob_name, levels = pp_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name) |>
  select(-c(presenting_prob_code, presenting_prob)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_alltime"))


####### QUARTERLY ########

#Presenting problem 1
pp1_qr <- df_refs |>
  group_by(dataset_type, presenting_prob_1, hb_name, ref_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_1) |
      presenting_prob_1 == "99" |
      presenting_prob_1 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_1, presenting_prob, ref_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_1') |>
  rename(presenting_prob_code = presenting_prob_1) |>
  arrange(hb_name) 

#Presenting problem 2
pp2_qr <- df_refs |>
  group_by(dataset_type, presenting_prob_2, hb_name, ref_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_2) |
      presenting_prob_2 == "99" |
      presenting_prob_2 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_2, presenting_prob, ref_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_2') |>
  rename(presenting_prob_code = presenting_prob_2) |>
  arrange(hb_name)

#Presenting problem 3
pp3_qr <- df_refs |>
  group_by(dataset_type, presenting_prob_3, hb_name, ref_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_3) |
      presenting_prob_3 == "99" |
      presenting_prob_3 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_3, presenting_prob, ref_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_3') |>
  rename(presenting_prob_code = presenting_prob_3) |>
  arrange(hb_name)

pp_qr <- rbind(pp1_qr, pp2_qr, pp3_qr)

pp_totals_qr <- pp_qr |>
  group_by(presenting_prob, hb_name, dataset_type, ref_quarter_ending) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, ref_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = presenting_prob, values_from = c("totals", "perc_refs")) |>
  mutate(across(all_refs:totals_valid_reason, ~prettyNum(., big.mark = ","))) |>
  arrange(hb_name, dataset_type, ref_quarter_ending) #|>
  #save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_qr"))


pp_breakdown_qr <- left_join(pp_qr, pp_lookup, by = "presenting_prob_code") |> 
  mutate(present_prob_name = factor(present_prob_name, levels = pp_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, ref_quarter_ending) |>
  select(-c(presenting_prob_code, presenting_prob)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_qr"))


####### MONTHLY ########

#Presenting problem 1
pp1_mth <- df_refs |>
  group_by(dataset_type, presenting_prob_1, hb_name, ref_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_1) |
      presenting_prob_1 == "99" |
      presenting_prob_1 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_1, presenting_prob, ref_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_1') |>
  rename(presenting_prob_code = presenting_prob_1) |>
  arrange(hb_name) 

#Presenting problem 2
pp2_mth <- df_refs |>
  group_by(dataset_type, presenting_prob_2, hb_name, ref_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_2) |
      presenting_prob_2 == "99" |
      presenting_prob_2 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_2, presenting_prob, ref_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_2') |>
  rename(presenting_prob_code = presenting_prob_2) |>
  arrange(hb_name)

#Presenting problem 3
pp3_mth <- df_refs |>
  group_by(dataset_type, presenting_prob_3, hb_name, ref_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_3) |
      presenting_prob_3 == "99" |
      presenting_prob_3 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_3, presenting_prob, ref_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'presenting_prob_3') |>
  rename(presenting_prob_code = presenting_prob_3) |>
  arrange(hb_name)

pp_mth <- rbind(pp1_mth, pp2_mth, pp3_mth)

pp_totals_mth <- pp_mth |>
  group_by(presenting_prob, hb_name, dataset_type, ref_month) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, ref_month) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = presenting_prob, values_from = c("totals", "perc_refs")) |>
  mutate(across(all_refs:totals_valid_reason, ~prettyNum(., big.mark = ","))) |>
  arrange(hb_name, dataset_type, ref_month) #|>
#save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_mth"))


pp_breakdown_mth <- left_join(pp_mth, pp_lookup, by = "presenting_prob_code") |> 
  mutate(present_prob_name = factor(present_prob_name, levels = pp_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, ref_month) |>
  select(-c(presenting_prob_code, presenting_prob)) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/presenting_prob_mth"))




