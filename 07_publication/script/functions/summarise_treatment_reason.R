##############################################.
### For publication - reason for treatment ###
##############################################.

# Author: Luke Taylor
# Date: 2025-10-23

#This script counts unique treatment reason codes in a patient pathway.
#Each distinct treatment reason code will be reported on based on the first
#month in which that code was returned. An example for a single patient 
#pathway is provided below.
#F100 01/03/2025
#F100 01/04/2025
#F100 01/05/2025 
#F45  01/06/2025
#In this case F100 would be counted in 03/25, and F45 in 06/25.
#Treatment reason codes are not double counted in multiple months.
#Separate counts are provided for treatment reason field 1,2 and 3.

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

summarise_treat_reason <- function(){
  
  treat_reason_dir <- paste0(shorewise_pub_data_dir, "/treat_reason/")
  dir.create(treat_reason_dir)
  measure_label <- "treat_reason_"

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

create_treat_reason_df <- function(treat_reason = c('treat_reason_1', 'treat_reason_2', 'treat_reason_3')){
  
  df_treat_reason <- df |>
    remove_borders_int_refs() |>
    mutate(treat_start_date = case_when(is.na(treat_start_date) ~ header_date,
                                        TRUE ~ treat_start_date), #use header date if treat_start_date is missing
           treat_month = floor_date(treat_start_date, unit = "month"),
           treat_quarter = ceiling_date(treat_month, unit = "quarter") - 1,
           treat_quarter_ending = floor_date(treat_quarter, unit = "month")) |>
    filter(treat_start_date %in% date_range, #using header date rather than ref month
           !is.na(.data[[treat_reason]])) |>
    select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, all_of(treat_reason), treat_month, 
           treat_quarter_ending, treat_start_date, header_date) |>
    arrange(ucpn, .data[[treat_reason]], treat_start_date, header_date) |>
    group_by(!!!syms(data_keys), .data[[treat_reason]]) |>
    slice_head(n = 1) |>
    ungroup()
  
}

df_treat_reason_1 <- create_treat_reason_df('treat_reason_1')
df_treat_reason_2 <- create_treat_reason_df('treat_reason_2')
df_treat_reason_3 <- create_treat_reason_df('treat_reason_3')


####### ALL TIME #######

#Reason for treatment 1
treat_reason_1_all <- df_treat_reason_1 |>
  filter(!is.na(treat_reason_1)) |>
  group_by(dataset_type, treat_reason_1, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_1) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_1 = gsub("[[:punct:]]", "", treat_reason_1),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Primary') |>
  rename(treat_reason_code = treat_reason_1) |>
  arrange(dataset_type, hb_name) 

#Reason for treatment 2
treat_reason_2_all <- df_treat_reason_2 |>
  filter(!is.na(treat_reason_2)) |>
  group_by(dataset_type, treat_reason_2, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_2) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_2 = gsub("[[:punct:]]", "", treat_reason_2),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Secondary') |>
  rename(treat_reason_code = treat_reason_2) |>
  arrange(dataset_type, hb_name)

#Reason for treatment 3
treat_reason_3_all <- df_treat_reason_3 |>
  filter(!is.na(treat_reason_3)) |>
  group_by(dataset_type, treat_reason_3, hb_name) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_3) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_3 = gsub("[[:punct:]]", "", treat_reason_3),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Tertiary') |>
  rename(treat_reason_code = treat_reason_3) |>
  arrange(dataset_type, hb_name)

treat_reason_all <- rbind(treat_reason_1_all, treat_reason_2_all, treat_reason_3_all) 

treat_reason_totals_all <- treat_reason_all |>
  group_by(treat_reason_code, hb_name, dataset_type) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_reason_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type)


#### Get full breakdown of reason for treatment ######
# Lookup codes for reason for treatment

treat_reason_lookup <- read_csv('/conf/linkage/output/lookups/Unicode/National Reference Files/icd10.csv') |>
  select(treat_reason_code = code1, v5, treat_reason_desc = Description) |>
  group_by(treat_reason_code, v5) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(treat_reason_desc = str_to_sentence(treat_reason_desc),
         treat_reason_code = case_when(is.na(v5) ~ treat_reason_code,
                                       TRUE ~ paste(treat_reason_code, v5, sep = ""))) |>
  select(-v5)

treat_reason_breakdown_all <- left_join(treat_reason_all, treat_reason_lookup, by = "treat_reason_code") |>
  ungroup() |>
  arrange(dataset_type, hb_name) |>
  select(dataset_type, hb_name, treat_reason_code, treat_reason_desc, n, level) |>
  save_as_parquet(paste0(treat_reason_dir, measure_label, "alltime"))


####### QUARTERLY ########

#Reason for treatment 1
treat_reason_1_qr <- df_treat_reason_1 |>
  filter(!is.na(treat_reason_1)) |>
  group_by(dataset_type, treat_reason_1, hb_name, treat_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_1, treat_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_1 = gsub("[[:punct:]]", "", treat_reason_1),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Primary') |>
  rename(treat_reason_code = treat_reason_1) |>
  arrange(dataset_type, hb_name, treat_quarter_ending) 

#Reason for treatment 2
treat_reason_2_qr  <- df_treat_reason_2 |>
  filter(!is.na(treat_reason_2)) |>
  group_by(dataset_type, treat_reason_2, hb_name, treat_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_2, treat_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_2 = gsub("[[:punct:]]", "", treat_reason_2),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Secondary') |>
  rename(treat_reason_code = treat_reason_2) |>
  arrange(dataset_type, hb_name, treat_quarter_ending)

#Reason for treatment 3
treat_reason_3_qr  <- df_treat_reason_3 |>
  filter(!is.na(treat_reason_3)) |>
  group_by(dataset_type, treat_reason_3, hb_name, treat_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_3, treat_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_3 = gsub("[[:punct:]]", "", treat_reason_3),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Tertiary') |>
  rename(treat_reason_code = treat_reason_3) |>
  arrange(dataset_type, hb_name, treat_quarter_ending)

treat_reason_qr  <- rbind(treat_reason_1_qr , treat_reason_2_qr , treat_reason_3_qr)

treat_reason_totals_qr <- treat_reason_qr  |>
  group_by(treat_reason_code, hb_name, dataset_type, treat_quarter_ending) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, treat_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_reason_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type, treat_quarter_ending) 


treat_reason_breakdown_qr <- left_join(treat_reason_qr, treat_reason_lookup, by = "treat_reason_code") |>
  ungroup() |>
  arrange(dataset_type, hb_name, treat_quarter_ending) |>
  select(dataset_type, hb_name, treat_quarter_ending, treat_reason_code, treat_reason_desc, n, level) |>
  save_as_parquet(paste0(treat_reason_dir, measure_label, "qr"))


####### MONTHLY ########

#Reason for treatment 1
treat_reason_1_mth <- df_treat_reason_1 |>
  filter(!is.na(treat_reason_1)) |>
  group_by(dataset_type, treat_reason_1, hb_name, treat_month) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_1, treat_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_1 = gsub("[[:punct:]]", "", treat_reason_1),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Primary') |>
  rename(treat_reason_code = treat_reason_1) |>
  arrange(dataset_type, hb_name, treat_month) 

#Reason for treatment 2
treat_reason_2_mth  <- df_treat_reason_2 |>
  filter(!is.na(treat_reason_2)) |>
  group_by(dataset_type, treat_reason_2, hb_name, treat_month) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_2, treat_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_2 = gsub("[[:punct:]]", "", treat_reason_2),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Secondary') |>
  rename(treat_reason_code = treat_reason_2) |>
  arrange(dataset_type, hb_name, treat_month)

#Reason for treatment 3
treat_reason_3_mth  <- df_treat_reason_3 |>
  filter(!is.na(treat_reason_3)) |>
  group_by(dataset_type, treat_reason_3, hb_name, treat_month) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(dataset_type, treat_reason_3, treat_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(treat_reason_3 = gsub("[[:punct:]]", "", treat_reason_3),
         hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Tertiary') |>
  rename(treat_reason_code = treat_reason_3) |>
  arrange(dataset_type, hb_name, treat_month)

treat_reason_mth  <- rbind(treat_reason_1_mth, treat_reason_2_mth, treat_reason_3_mth) 

treat_reason_totals_mth <- treat_reason_mth  |>
  group_by(treat_reason_code, hb_name, dataset_type, treat_month) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, treat_month) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = treat_reason_code, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type, treat_month) 


treat_reason_breakdown_mth <- left_join(treat_reason_mth, treat_reason_lookup, by = "treat_reason_code") |>
  ungroup() |>
  arrange(dataset_type, hb_name, treat_month) |>
  select(dataset_type, hb_name, treat_month, treat_reason_code, treat_reason_desc, n, level) |>
  save_as_parquet(paste0(treat_reason_dir, measure_label, "mth"))

}
