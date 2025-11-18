#################################.
### For publication - CGI/PGI ###
#################################.

# Author: Luke Taylor
# Date: 2025-11-17

#This script counts clinical outcome measure records based on header date.
#Records are not currently associated with an appointment date.
#Best we can do is count the number returned each submission month.


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

summarise_clinical_outcomes <- function(){
  
  clinical_outcomes_dir <- paste0(shorewise_pub_data_dir, "/clinical_outcomes/")
  dir.create(clinical_outcomes_dir)
  measure_label <- "clinical_outcomes_"

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
outcome_measures <- c("cgi_i", "pgi_i", "cgi_s")

#all appts + clinical outcome measures
df_clinical_out <- df |>
  remove_borders_int_refs() |>
  filter(!is.na(cgi_i) | !is.na(pgi_i) | !is.na(cgi_s)) |> 
  filter(header_date %in% date_range) |>
  select(all_of(data_keys), all_of(demographics), all_of(outcome_measures),header_date) |>
  arrange(ucpn, header_date) 


#Monthly
#cgi-i
cgi_i_df <- df_clinical_out |>
  filter(!is.na(cgi_i)) |>
  group_by(dataset_type, hb_name, header_date, cgi_i) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(dataset_type, header_date, cgi_i) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         outcome = 'CGI-I') |>
  rename(outcome_code = cgi_i) |>
  arrange(dataset_type, hb_name, header_date) |>
  save_as_parquet(paste0(clinical_outcomes_dir, measure_label, "cgi_i_monthly"))


#pgi-i
pgi_i_df <- df_clinical_out |>
  filter(!is.na(pgi_i)) |>
  group_by(dataset_type, hb_name, header_date, pgi_i) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(dataset_type, header_date, pgi_i) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         outcome = 'PGI-I') |>
  rename(outcome_code = pgi_i) |>
  arrange(dataset_type, hb_name, header_date) |>
  save_as_parquet(paste0(clinical_outcomes_dir, measure_label, "pgi_i_monthly"))


#cgi-s
cgi_s_df <- df_clinical_out |>
  filter(!is.na(cgi_s)) |>
  group_by(dataset_type, hb_name, header_date, cgi_s) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(dataset_type, header_date, cgi_s) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         outcome = 'CGI-S') |>
  rename(outcome_code = cgi_s) |>
  arrange(dataset_type, hb_name, header_date) |>
  save_as_parquet(paste0(clinical_outcomes_dir, measure_label, "cgi_s_monthly"))
  
}






