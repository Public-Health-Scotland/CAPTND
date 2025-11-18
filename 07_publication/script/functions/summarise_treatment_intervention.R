#########################################################.
### For publication - treatment intervention received ###
#########################################################.

# Author: Luke Taylor
# Date: 2025-10-22

#This script counts unique treatment intervention codes in a patient pathway.
#Each distinct treatment intervention code will be reported on based on the first
#month in which that code was returned. An example for a single patient 
#pathway is provided below.
#01X 01/03/2025
#01X 01/04/2025
#01X 01/05/2025 
#08B  01/06/2025
#In this case 01X would be counted in 03/25, and 08B in 06/25.
#Treatment intervention codes are not double counted in multiple months.
#Separate counts are provided for treatment reason field 1,2 and 3.
#Not known (99) and Not applicable (96) are removed from counts


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

summarise_treat_intervention <- function(){
  
  treat_intervention_dir <- paste0(shorewise_pub_data_dir, "/treat_intervention/")
  dir.create(treat_intervention_dir)
  measure_label <- "treat_intervention_"

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
    mutate(treat_start_date = case_when(is.na(treat_start_date) ~ header_date,
                                        TRUE ~ treat_start_date), #use header date if treat_start_date is missing
           treat_month = floor_date(treat_start_date, unit = "month"),
           treat_quarter = ceiling_date(treat_month, unit = "quarter") - 1,
           treat_quarter_ending = floor_date(treat_quarter, unit = "month")) |>
    filter(treat_start_date %in% date_range, #using header date rather than ref month
           !is.na(.data[[treat_type]]) & .data[[treat_type]] != '99' & .data[[treat_type]] != '96') |>
    select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, all_of(treat_type), treat_month,
           treat_quarter_ending, treat_start_date, header_date) |>
    arrange(ucpn, .data[[treat_type]], treat_start_date, header_date) |>
    group_by(!!!syms(data_keys), .data[[treat_type]]) |>
    slice_head(n = 1) |>
    ungroup()
  
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
         level = 'Primary') |>
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
         level = 'Secondary') |>
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
         level = 'Tertiary') |>
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
  arrange(hb_name, dataset_type) 


#### Get full breakdown of treatment intervention ######
# Lookup codes for treatment intervention

treat_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Treatment_update") %>% 
  mutate(Codes = as.factor(Codes)) |>
  rename(treat_name = Values,
         treat_code = Codes) |>
  select(1:2) |>
  #make sentence string except brackets
  mutate(treat_name = map_chr(treat_name, ~ {
    bracketed <- str_extract_all(.x, "\\([^)]*\\)|\\[[^]]*\\]")[[1]]
    cleaned <- str_remove_all(.x, "\\([^)]*\\)|\\[[^]]*\\]")
    cleaned <- str_to_sentence(str_trim(cleaned))
    paste(cleaned, paste(bracketed, collapse = " "))
  })) |>
  mutate(treat_name = case_when(treat_name == "Cbt based parenting programme " ~ "CBT based parenting programme",
                                treat_name == "Cbt for psychosis (CBT_P)" ~ "CBT for psychosis (CBT_P)",
                                TRUE ~ treat_name),
         treat_code = str_remove(treat_code, "^0+"),
         treat_name_long = paste(treat_code, treat_name, sep = " - "))

#set order of reason codes
treat_order <- treat_lookup$treat_name_long

treat_breakdown_all <- left_join(treat_all, treat_lookup, by = "treat_code") |>
  mutate(treat_name_long = factor(treat_name_long, levels = treat_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name) |>
  select(dataset_type, hb_name, treat_name_long, n, level) |>
  save_as_parquet(paste0(treat_intervention_dir, measure_label, "alltime"))


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
         level = 'Primary') |>
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
         level = 'Secondary') |>
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
         level = 'Tertiary') |>
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
  arrange(hb_name, dataset_type, treat_quarter_ending) 


treat_breakdown_qr <- left_join(treat_qr, treat_lookup, by = "treat_code") |>
  mutate(treat_name_long = factor(treat_name_long, levels = treat_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, treat_quarter_ending) |>
  select(dataset_type, hb_name, treat_quarter_ending, treat_name_long, n, level) |>
  save_as_parquet(paste0(treat_intervention_dir, measure_label, "qr"))


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
         level = 'Primary') |>
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
         level = 'Secondary') |>
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
         level = 'Tertiary') |>
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
  save_as_parquet(paste0(treat_intervention_dir, measure_label, "mth"))

}
