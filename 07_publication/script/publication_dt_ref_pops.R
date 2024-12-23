########################################.
### Make population reference tables ###
########################################.

# Author: Luke Taylor
# Date: 2024-12-19

#Most recent year - 2022

ref_pops_dir <- paste0(shorewise_pub_data_dir, "/reference_pops/")
dir.create(ref_pops_dir)

measure_label <- 'ref_pops_'

df_datazone_pop <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2022.rds') |> 
  filter(year == max(year)) |> # latest year is 2021 population
  select(hb2019name, simd2020v2_sc_quintile, sex, 5:95) |> 
  pivot_longer(cols = 4:94, names_to = 'age', values_to = 'count') |> 
  mutate(age = str_remove(age, c("age")),
         age = as.numeric(str_remove(age, c("plus"))),
         dataset_type = "PT") |> 
  rename(hb_name = hb2019name, 
         simd2020_quintile = simd2020v2_sc_quintile)

df_all <- df_datazone_pop |> 
  filter(age <= 18) |> 
  mutate(dataset_type = "CAMHS") |> 
  rbind(df_datazone_pop) 

# hb pop by dataset
df_ds_hb_pop <- df_all |> 
  group_by(dataset_type, hb_name) |> 
  summarise(tot_population = sum(count)) |> 
  group_by(dataset_type) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop"))  |> 
  arrange(dataset_type, hb_name) |> 
  save_as_parquet(path = paste0(ref_pops_dir, measure_label, "hb_totals"))

# hb by sex pop
df_ds_hb_sex_pop <- df_all |>
  mutate(sex = case_when(sex == 'M' ~ 'Male',
                         sex == 'F' ~ 'Female')) |>
  group_by(dataset_type, hb_name, sex) |> 
  summarise(population = sum(count)) |> 
  group_by(dataset_type, sex) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop"))  |> 
  arrange(dataset_type, hb_name) |> 
  save_as_parquet(path = paste0(ref_pops_dir, measure_label, "hb_sex_totals"))

# hb by age group pop
df_ds_hb_age_grp_pop <- df_all |> 
  mutate(age_group = case_when(dataset_type == 'PT' & age <= 24 ~ 'Under 25',
                               dataset_type == 'PT' & age >= 25 & age <= 39 ~ '25-39',
                               dataset_type == 'PT' & age >= 40 & age <= 64 ~ '40-64',
                               dataset_type == 'PT' & age >= 65 ~ '65 plus',
                               dataset_type == 'CAMHS' & age < 6 ~ 'Under 6',
                               dataset_type == 'CAMHS' & age >= 6 & age <= 11 ~ '6-11',
                               dataset_type == 'CAMHS' & age >= 12 & age <= 15 ~ '12-15',
                               dataset_type == 'CAMHS' & age > 15 ~ 'Over 15')) |>
  group_by(dataset_type, hb_name, age_group) |> 
  summarise(population = sum(count, na.rm = TRUE)) |> 
  group_by(dataset_type, age_group) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop"))  |> 
  arrange(dataset_type, hb_name) |> 
  save_as_parquet(path = paste0(ref_pops_dir, measure_label, "hb_age_group_totals"))


# hb by simd pop
df_ds_hb_simd_pop <- df_all |> 
  mutate(simd2020_quintile = as.character(simd2020_quintile),
         simd2020_quintile = case_when(simd2020_quintile == 1 ~ '1 - Most Deprived',
                                       simd2020_quintile == 5 ~ '5 - Least Deprived',
                                       TRUE ~ simd2020_quintile)) |>
  group_by(dataset_type, hb_name, simd2020_quintile) |> 
  summarise(population = sum(count)) |> 
  group_by(dataset_type, simd2020_quintile) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop"))  |> 
  arrange(dataset_type, hb_name) |> 
  save_as_parquet(path = paste0(ref_pops_dir, measure_label, "hb_simd_totals"))

