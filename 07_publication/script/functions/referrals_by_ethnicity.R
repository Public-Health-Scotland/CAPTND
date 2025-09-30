################################################.
### For publication - referrals by ethnicity ###
################################################.

# Author: Bex Madden
# Date: 2024-11-26


summarise_referrals_ethnicity <- function(){
  
ref_eth_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/")
dir.create(ref_eth_dir)
measure_label <- "referrals_ethnicity_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  group_by(!!!syms(data_keys)) |> 
  arrange(!!sym(app_date_o), .by_group = TRUE) |>
  fill(ethnicity_last_reported, .direction = "downup") |>
  slice(1) |> 
  ungroup() |> 
  remove_borders_int_refs() |> 
  add_sex_description() |> 
  tidy_age_group_order()

# lookup codes for ethnicity
eth_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Ethnicity_code_update") %>% 
  select(-Ethnic_Group) |>
  rename(ethnicity_last_reported = ETHNICITY,
         eth_label = Ethnic,
         eth_group = Group) |> 
  mutate(eth_group = case_when(eth_group == "A" ~ "White",
                                eth_group == "B" ~ "Mixed/Multiple",
                                eth_group == "C" ~ "Asian (inc Scottish/British)",
                                eth_group == "D" ~ "African (inc Scottish/British)",
                                eth_group == "E" ~ "Caribbean or Black",
                                eth_group == "F" ~ "Other ethnic group",
                                eth_group == "G" ~ "Prefer not to say", #based on new PHS guidance
                               TRUE ~ "Not known"))

#set order of reason codes
eth_order <- eth_lookup$eth_label
eth_grp_order <- unique(eth_lookup$eth_group)

eth_census_figs <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Ethnicity/hb_ethnicity_census_2022.rds')

eth_pop_lookup <- eth_census_figs |> 
  filter(sex == 'All people',
         age_group == 'Total') |>
  mutate(dataset_type = 'PT')
  

eth_pop_lookup <- eth_census_figs |> 
  filter(sex == 'All people',
         age_group %in% c("0 - 4", "5 - 9", "10 - 14", "15", "16 - 17")) |> 
  mutate(dataset_type = "CAMHS") |> 
  rbind(eth_pop_lookup) |>
  left_join(eth_lookup, by = c("ethnic_group_code" = "ethnicity_last_reported")) |>
  group_by(eth_group, dataset_type, area) |>
  mutate(pop = sum(population),
         hb_name = paste0("NHS ", area)) |>
  ungroup() |>
  select(dataset_type, hb_name, eth_group, pop) |>
  distinct() |>
  group_by(eth_group, dataset_type) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop"))

tot_census_pop <- eth_pop_lookup |>
  group_by(hb_name, dataset_type) %>%
  summarise(pop = sum(pop)) |>
  mutate(eth_group = 'Not known')

eth_pop_lookup <- rbind(eth_pop_lookup, tot_census_pop)
  
  
# overall -----------------------------------------------------------------

# by hb
df_all_hb <- df_single_row |> 
  filter(!!sym(referral_month_o) %in% date_range) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ethnicity_last_reported_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(ethnicity_last_reported_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  #add_proportion_ds_hb() |> 
  left_join(eth_lookup, by = "ethnicity_last_reported") |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         eth_label = factor(eth_label, levels = eth_order),
         eth_group = factor(eth_group, levels = eth_grp_order)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "all_hb")) |> 
  
  # by group
  ungroup() |> 
  mutate(eth_group = replace_na(eth_group, "Not known")) |> 
  group_by(dataset_type, hb_name, eth_group) |> 
  summarise(count = sum(count)) |> 
  left_join(eth_pop_lookup, by = c('hb_name', 'eth_group', 'dataset_type')) |>
  mutate(rate_per_1000 = round(count / pop * 1000, 1)) |>
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_all_hb"))

# even broken down by hb there are disclosive numbers
# no HB

eth_pop_lookup_scot <- eth_pop_lookup |>
  filter(hb_name == 'NHS Scotland')

df_all <- df_single_row |> 
  filter(!!sym(referral_month_o) %in% date_range) |>
  group_by(!!sym(dataset_type_o), !!sym(ethnicity_last_reported_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  left_join(eth_lookup, by = "ethnicity_last_reported") |> 
  mutate(eth_label = factor(eth_label, levels = eth_order),
         eth_group = factor(eth_group, levels = eth_grp_order)) |> 
  arrange(!!sym(dataset_type_o)) |> 
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "all")) |> 
  
  # by group
  ungroup() |> 
  mutate(eth_group = replace_na(eth_group, "Not known")) |> 
  group_by(dataset_type, eth_group) |> 
  summarise(count = sum(count)) |> 
  left_join(eth_pop_lookup_scot, by = c('eth_group', 'dataset_type')) |>
  mutate(rate_per_1000 = round(count / pop * 1000, 1)) |>
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_all"))


# no HB QUARTERLY

df_all_qt <- df_single_row |> 
  filter(!!sym(referral_month_o) %in% date_range) |>
  group_by(!!sym(dataset_type_o), !!sym(ethnicity_last_reported_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  left_join(eth_lookup, by = "ethnicity_last_reported") |> 
  mutate(eth_label = factor(eth_label, levels = eth_order),
         eth_group = factor(eth_group, levels = eth_grp_order)) |> 
  arrange(!!sym(dataset_type_o), ref_quarter_ending) |> 
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "all_qt")) |> 
  
  # by group
  ungroup() |> 
  mutate(eth_group = replace_na(eth_group, "Not known")) |> 
  group_by(dataset_type, eth_group, ref_quarter_ending) |> 
  summarise(count = sum(count)) |> 
  left_join(eth_pop_lookup_scot, by = c('eth_group', 'dataset_type')) |>
  mutate(rate_per_1000 = round(count / pop * 1000, 1)) |>
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_all_qt"))

#All time
df_all_time_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ethnicity_last_reported_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(ethnicity_last_reported_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  #add_proportion_ds_hb() |> 
  left_join(eth_lookup, by = "ethnicity_last_reported") |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         eth_label = factor(eth_label, levels = eth_order),
         eth_group = factor(eth_group, levels = eth_grp_order)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "alltime_hb")) |> 
  
  # by group
  ungroup() |> 
  mutate(eth_group = replace_na(eth_group, "Not known")) |> 
  group_by(dataset_type, hb_name, eth_group) |> 
  summarise(count = sum(count)) |> 
  left_join(eth_pop_lookup, by = c('hb_name', 'eth_group', 'dataset_type')) |>
  mutate(rate_per_1000 = round(count / pop * 1000, 1)) |>
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_alltime_hb"))


}


