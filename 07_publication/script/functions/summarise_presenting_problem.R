#########################################################.
### For publication - referrals by presenting problem ###
#########################################################.

# Author: Luke Taylor
# Date: 2025-10-22

#This script counts presenting problem codes for the first contact appointment in a
#patient pathway. This should eventually be updated to the first attended contact in
#the pathway.
#Separate counts are provided for presenting problem field 1,2 and 3.


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

summarise_presenting_prob <- function(){

#### SETUP #####
  
  present_prob_dir <- paste0(shorewise_pub_data_dir, "/present_prob/")
  dir.create(present_prob_dir)
  measure_label <- "presenting_prob_"

# load data
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  remove_borders_int_refs() |>
  
  mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
         app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(app_quarter, unit = "month"))

demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "presenting_prob_1",
                  "presenting_prob_2", "presenting_prob_3")

df_app <- df |>
  select(all_of(data_keys), app_date, app_month, att_status, att_cat, app_purpose,
         all_of(demographics), ref_acc, app_quarter_ending, ref_acc_last_reported, app_quarter_ending) |> 
  filter(!is.na(!!sym(app_date_o)))  

#Presenting health problem at first appointment

df_refs <- df_app |>
  #filter(att_status == 1) |> #first appt would have to be attended to determine presenting problem?
  #high number of valid codes for non-attended appts, therefore removed for now
  arrange(!!!syms(data_keys), !!sym(app_date_o)) |> 
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |> #slice first appt
  ungroup() |>
  filter(app_month %in% date_range) |>
  select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, app_quarter_ending,
         app_month, app_date, att_status) |>
  mutate(presenting_prob_1 = as.numeric(presenting_prob_1),
         presenting_prob_2 = as.numeric(presenting_prob_2),
         presenting_prob_3 = as.numeric(presenting_prob_3)) |>
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
         level = 'Primary') |>
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
         level = 'Secondary') |>
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
         level = 'Tertiary') |>
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
  arrange(hb_name, dataset_type)


#### Get full breakdown of presenting problem ######
# Lookup codes for presenting problem

pp_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Presenting_Problem") %>% 
  select(-Further.Information) |>
  #mutate(Codes = as.factor(Codes)) |>
  rename(present_prob_name = Values,
         presenting_prob_code = Codes) |>
  mutate(present_prob_name = str_to_sentence(present_prob_name),
         present_prob_name = case_when(present_prob_name == 'Adhd' ~ 'ADHD',
                               present_prob_name == 'Fasd assessment' ~ 'FASD Assessment',
                               present_prob_name == 'Gender identity / lgbt issues' ~ 'Gender identity / LGBT issues',
                               TRUE ~ present_prob_name))

#set order of reason codes
pp_order <- pp_lookup$present_prob

pp_breakdown_all <- left_join(pp_all, pp_lookup, by = "presenting_prob_code") |> 
  mutate(present_prob_name = factor(present_prob_name, levels = pp_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name) |>
  select(-c(presenting_prob_code, presenting_prob)) |>
  save_as_parquet(paste0(present_prob_dir, measure_label, "alltime"))


####### QUARTERLY ########

#Presenting problem 1
pp1_qr <- df_refs |>
  group_by(dataset_type, presenting_prob_1, hb_name, app_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_1) |
      presenting_prob_1 == "99" |
      presenting_prob_1 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_1, presenting_prob, app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Primary') |>
  rename(presenting_prob_code = presenting_prob_1) |>
  arrange(hb_name) 

#Presenting problem 2
pp2_qr <- df_refs |>
  group_by(dataset_type, presenting_prob_2, hb_name, app_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_2) |
      presenting_prob_2 == "99" |
      presenting_prob_2 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_2, presenting_prob, app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Secondary') |>
  rename(presenting_prob_code = presenting_prob_2) |>
  arrange(hb_name)

#Presenting problem 3
pp3_qr <- df_refs |>
  group_by(dataset_type, presenting_prob_3, hb_name, app_quarter_ending) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_3) |
      presenting_prob_3 == "99" |
      presenting_prob_3 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_3, presenting_prob, app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Tertiary') |>
  rename(presenting_prob_code = presenting_prob_3) |>
  arrange(hb_name)

pp_qr <- rbind(pp1_qr, pp2_qr, pp3_qr)

pp_totals_qr <- pp_qr |>
  group_by(presenting_prob, hb_name, dataset_type, app_quarter_ending) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, app_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = presenting_prob, values_from = c("totals", "perc_refs")) |>
  mutate(across(all_refs:totals_valid_reason, ~prettyNum(., big.mark = ","))) |>
  arrange(hb_name, dataset_type, app_quarter_ending)


pp_breakdown_qr <- left_join(pp_qr, pp_lookup, by = "presenting_prob_code") |> 
  mutate(present_prob_name = factor(present_prob_name, levels = pp_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, app_quarter_ending) |>
  select(-c(presenting_prob_code, presenting_prob)) |>
  save_as_parquet(paste0(present_prob_dir, measure_label, "qr"))


####### MONTHLY ########

#Presenting problem 1
pp1_mth <- df_refs |>
  group_by(dataset_type, presenting_prob_1, hb_name, app_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_1) |
      presenting_prob_1 == "99" |
      presenting_prob_1 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_1, presenting_prob, app_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Primary') |>
  rename(presenting_prob_code = presenting_prob_1) |>
  arrange(hb_name) 

#Presenting problem 2
pp2_mth <- df_refs |>
  group_by(dataset_type, presenting_prob_2, hb_name, app_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_2) |
      presenting_prob_2 == "99" |
      presenting_prob_2 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_2, presenting_prob, app_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Secondary') |>
  rename(presenting_prob_code = presenting_prob_2) |>
  arrange(hb_name)

#Presenting problem 3
pp3_mth <- df_refs |>
  group_by(dataset_type, presenting_prob_3, hb_name, app_month) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(presenting_prob = case_when(
    is.na(presenting_prob_3) |
      presenting_prob_3 == "99" |
      presenting_prob_3 == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason")) |>
  group_by(dataset_type, presenting_prob_3, presenting_prob, app_month) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         level = 'Tertiary') |>
  rename(presenting_prob_code = presenting_prob_3) |>
  arrange(hb_name)

pp_mth <- rbind(pp1_mth, pp2_mth, pp3_mth)

pp_totals_mth <- pp_mth |>
  group_by(presenting_prob, hb_name, dataset_type, app_month) |>
  summarise(totals = sum(n), .groups = "drop") |>
  group_by(hb_name, dataset_type, app_month) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = presenting_prob, values_from = c("totals", "perc_refs")) |>
  mutate(across(all_refs:totals_valid_reason, ~prettyNum(., big.mark = ","))) |>
  arrange(hb_name, dataset_type, app_month) 


pp_breakdown_mth <- left_join(pp_mth, pp_lookup, by = "presenting_prob_code") |> 
  mutate(present_prob_name = factor(present_prob_name, levels = pp_order)) |>
  ungroup() |>
  arrange(dataset_type, hb_name, app_month) |>
  select(-c(presenting_prob_code, presenting_prob)) |>
  save_as_parquet(paste0(present_prob_dir, measure_label, "mth"))

}
