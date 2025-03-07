
###########################################.
###  Referrals by Source Investigation  ###
###########################################.

# Author: Luke Taylor
# Date: 2024-11-01

summarise_ref_source <- function(){

dir.create(ref_source_dir)
measure_label <- "ref_source_"


# 1 - open most recent RTT eval file-------------------------------------
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# 2 - referral source lookup---------------------------------------------

ref_source_lookup <- read_xlsx("../../../data/captnd_codes_lookup.xlsx",sheet = 'Ref_Source') %>%
  select(code = Code, ref_source_name = Ref_Source) |>
  mutate(ref_source_name = str_to_sentence(ref_source_name),
         ref_source_name = case_when(ref_source_name == 'Self referral (includes self, relations, friends and carers)' ~ 'Self referral',
                                     ref_source_name == 'A&e' ~ 'Accident & emergency',
                                     ref_source_name == 'Gp' ~ 'General practitioner',
                                     ref_source_name == 'Lac' ~ 'LAC',
                                     ref_source_name == 'Camhs team' ~ 'CAMHS team',
                                     ref_source_name == 'Camhs – out of area' ~ 'CAMHS - Out of area',
                                     ref_source_name == 'Nhs24' ~ 'NHS24',
                                     ref_source_name == 'Consultant outwith own hb' ~ 'Consultant outside own health board',
                                     TRUE ~ ref_source_name))

# 3 - ensure one row per ref---------------------------------------------
df_ref_source <- df %>%
  filter(!(!!sym(hb_name_o) == 'NHS Greater Glasgow and Clyde' & !!sym(dataset_type_o) == 'PT' &
             !!sym(referral_month_o) < '2024-10-01')) |> #due to known coding inaccuracies before this point
  filter(!!sym(referral_month_o) %in% month_range) |>
  group_by(!!!syms(data_keys)) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(all_of(data_keys), !!sym(ref_source_o), !!sym(referral_month_o),
         !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o)) |>
  left_join(ref_source_lookup, by = join_by(!!sym(ref_source_o) == code)) |>
  mutate(ref_source_name = case_when(is.na(!!sym(ref_source_o)) ~ 'Missing data',
                                     TRUE ~ ref_source_name)) |> #NAs to Data Missing
  add_sex_description() 
  
#by month/quarter and hb-------------------------------------------------
ref_source_mth_hb <- df_ref_source |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
           ref_source_name) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), ref_source_name) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "month_hb")) |>
  
  append_quarter_ending(date_col = "referral_month") |>
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "ref_source_name")) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "quarter_hb")) 
  
#by month/quarter, hb and sex------------------------------------------- 
ref_source_hb_sex <- df_ref_source |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
           !!sym(sex_reported_o), ref_source_name) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o), ref_source_name) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name", "sex_reported")) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "month_hb_sex")) |>
  
  append_quarter_ending(date_col = "referral_month") |>
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                     "ref_source_name", "sex_reported")) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |>
  arrange(dataset_type, hb_name, sex_reported) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "quarter_hb_sex")) 

#by month/quarter, hb and age------------------------------------------- 
ref_source_hb_age <- df_ref_source |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
           !!sym(age_group_o), ref_source_name) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(age_group_o), ref_source_name) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name", "age_group")) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "month_hb_age")) |>
  
  append_quarter_ending(date_col = "referral_month") |>
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                     "ref_source_name", "age_group")) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name", "age_group")) |>
  arrange(dataset_type, hb_name, age_group) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "quarter_hb_age")) 

#by month/quarter, hb and simd------------------------------------------- 
ref_source_hb_simd <- df_ref_source |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
           !!sym(simd_quintile_o), ref_source_name) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(simd_quintile_o), ref_source_name) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name", "simd2020_quintile")) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "month_hb_simd")) |>
  
  append_quarter_ending(date_col = "referral_month") |>
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                     "ref_source_name", "simd2020_quintile")) |>
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |>
  arrange(dataset_type, hb_name, simd2020_quintile) |>
  save_as_parquet(path = paste0(ref_source_dir, measure_label, "quarter_hb_simd")) 

}







