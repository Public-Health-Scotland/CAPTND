################################################.
### For publication - referrals by ethnicity ###
################################################.

# Author: Bex Madden
# Date: 2024-11-26

ref_eth_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/")
dir.create(ref_eth_dir)
measure_label <- "referrals_ethnicity_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  ungroup() |> 
  as.data.frame() #|> 
  # add_sex_description() |> 
  # tidy_age_group_order()

# lookup codes for ethnicity
eth_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Ethnicity_code_update") %>% 
  select(-Ethnic_Group) |>
  rename(ethnicity_last_reported = ETHNICITY,
         eth_label = Ethnic,
         eth_group = Group) |> 
  mutate(eth_group = case_when(eth_group == "A" ~ "White",
                                eth_group == "B" ~ "Mixed/Multiple",
                                eth_group == "C" ~ "Asian (inc Scottish/British)",
                                eth_group == "D" ~ "African",
                                eth_group == "E" ~ "Caribbean or Black",
                                eth_group == "F" ~ "Other",
                                TRUE ~ "Not known"))

#set order of reason codes
eth_order <- eth_lookup$eth_label
eth_grp_order <- unique(eth_lookup$eth_group)

# overall -----------------------------------------------------------------

# by hb
df_all_hb <- df_single_row |> 
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
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_all_hb"))

# even broken down by hb there are disclosive numbers
# no HB

df_all <- df_single_row |> 
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
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_all"))


# no HB QUARTERLY

df_all_qt <- df_single_row |> 
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
  save_as_parquet(path = paste0(ref_eth_dir, measure_label, "grp_all_qt"))




# plot 
df_eth_plot <- df_all_qt |> 
  filter(ref_quarter_ending == max(ref_quarter_ending)) |> 
  group_by(dataset_type) |> 
  mutate(total_refs = sum(count),
         prop = round(count/total_refs*100, 1))


eth_plot <- df_eth_plot |> 
  ggplot(aes(x = eth_group, y = prop, color = dataset_type)) +
  geom_point() +
  coord_flip() +
  scale_y_sqrt(limits = c(0,100), breaks = seq(0,100, by=10)) +
  theme_minimal() +
  labs(title = "Ethnicity in CAPTND latest Quarter")

eth_plot
