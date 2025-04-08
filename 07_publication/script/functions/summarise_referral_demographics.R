###########################################
### Publication - referral demographics ###
###        Count/Rates per 1000         ###
###########################################

# Author: Luke Taylor
# Date: 2024-12-19

summarise_referral_demographics <- function(df){
  
source('./07_publication/script/publication_dt_ref_pops.R')

# create for saving output files in
ref_demo_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_demographics/")
dir.create(ref_demo_dir)
measure_label <- "referrals_"

# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
  #lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  add_sex_description() |> 
  tidy_age_group_order() |>
  remove_borders_int_refs()

# by hb, month -----------------------------------------------------------------
#reference pop
ref_pop_hb <- read_parquet(paste0(ref_pops_dir, "/ref_pops_hb_totals.parquet"))

df_month_hb <- df_single_row |> 
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "month_hb")) |>
  #by quarter
  append_quarter_ending(date_col = "referral_month") |> 
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |>  
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  #population rate
  left_join(ref_pop_hb, by = c("dataset_type", "hb_name")) |> 
  mutate(pop_rate_1000 = round(count / tot_population * 1000, 2)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "quarter_hb"))

# by hb, month, and sex --------------------------------------------------------

#updated df
updated_sex_groups_df <- df_single_row |>
  mutate(sex = case_when(!!sym(sex_o) == 0 ~ 'Not known',
                         !!sym(sex_o) == 9 ~ 'Not specified',
                         !!sym(sex_o) == 1 ~ 'Male',
                         !!sym(sex_o) == 2 ~ 'Female',
                         is.na(!!sym(sex_o)) ~ 'Data missing')) |>
  mutate(sex_reported = case_when(is.na(sex_reported) ~ sex,
                                  TRUE ~ sex_reported))

#reference pop
ref_pop_hb_sex <- read_parquet(paste0(ref_pops_dir, "/ref_pops_hb_sex_totals.parquet"))

df_month_hb_sex <- updated_sex_groups_df |> 
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
           !!sym(sex_reported_o)) |> 
  mutate(sex_reported = case_when(is.na(!!sym(sex_reported_o)) ~ 'Data Missing',
                                  TRUE ~ !!sym(sex_reported_o))) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "month_hb_sex")) |>
  #by quarter
  append_quarter_ending(date_col = "referral_month") |> 
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |> 
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  #population rate
  left_join(ref_pop_hb_sex, by = c("dataset_type", "hb_name", "sex_reported" = "sex")) |> 
  left_join(ref_pop_hb, by = c("dataset_type", "hb_name")) |>
  mutate(pop_rate_1000 = round(count / population * 1000, 2),
         tot_pop_rate_1000 = round(total / tot_population * 1000, 2)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "quarter_hb_sex"))


# by hb, month, and age --------------------------------------------------------

#update df
updated_age_groups_df <- df_single_row |>
  mutate(agg_age_groups = case_when(#PT age groups
    !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) <= 24 ~ 'Under 25',
    !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 25 & !!sym(age_at_ref_rec_o) <= 39 ~ '25-39',
    !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 40 & !!sym(age_at_ref_rec_o) <= 64 ~ '40-64',
    !!sym(dataset_type_o) == 'PT' & !!sym(age_at_ref_rec_o) >= 65 ~ '65 plus',
    #CAMHS age groups
    !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) < 6 ~ 'Under 6',
    !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) >= 6 & !!sym(age_at_ref_rec_o) <= 11 ~ '6-11',
    !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) >= 12 & !!sym(age_at_ref_rec_o) <= 15 ~ '12-15',
    !!sym(dataset_type_o) == 'CAMHS' & !!sym(age_at_ref_rec_o) > 15 ~ 'Over 15',
    #NAs with invalid CHI
    is.na(!!sym(dob_o)) & is.na(!!sym(age_at_ref_rec_o)) ~ 'Data missing'))

#reference pop
ref_pop_hb_age_groups <- read_parquet(paste0(ref_pops_dir, "/ref_pops_hb_age_group_totals.parquet"))

df_month_hb_age <- updated_age_groups_df |> 
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), agg_age_groups) |> 
  mutate(agg_age_groups = case_when(is.na(agg_age_groups) ~ 'Data Missing',
                                  TRUE ~ agg_age_groups)) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), agg_age_groups) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  right_join(df_age_mth_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name", "agg_age_groups")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "month_hb_age")) |>
  #by quarter
  append_quarter_ending(date_col = "referral_month") |> 
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "agg_age_groups")) |> 
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type","hb_name")) |> 
  arrange(agg_age_groups, !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  #population rate
  left_join(ref_pop_hb_age_groups, by = c("dataset_type", "hb_name", "agg_age_groups" = "age_group")) |> 
  left_join(ref_pop_hb, by = c("dataset_type", "hb_name")) |>
  mutate(pop_rate_1000 = round(count / population * 1000, 2),
         tot_pop_rate_1000 = round(total / tot_population * 1000, 2)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "quarter_hb_age")) 


# by hb, month, and simd -------------------------------------------------------

#updated df
updated_simd_df <- df_single_row |>
  mutate(simd2020_quintile = as.character(simd2020_quintile),
         simd2020_quintile = case_when(simd2020_quintile == 1 ~ '1 - Most Deprived',
                                       simd2020_quintile == 5 ~ '5 - Least Deprived',
                                       is.na(!!sym(postcode_last_reported_o)) ~ 'Data missing',
                                       !is.na(!!sym(postcode_last_reported_o)) & is.na(simd2020_quintile) ~ 'Not known',
                                       TRUE ~ simd2020_quintile))

#reference pop
ref_pop_hb_simd <- read_parquet(paste0(ref_pops_dir, "/ref_pops_hb_simd_totals.parquet"))

df_month_hb_simd <- updated_simd_df |> 
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
           !!sym(simd_quintile_o)) |>
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(simd_quintile_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  right_join(df_simd_mth_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name", "simd2020_quintile")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  mutate(simd2020_quintile = as.character(simd2020_quintile)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "month_hb_simd")) |> 
  #by quarter
  append_quarter_ending(date_col = "referral_month") |> 
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |> 
  add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  #population rate
  left_join(ref_pop_hb_simd, by = c("dataset_type", "hb_name", "simd2020_quintile")) |> 
  left_join(ref_pop_hb, by = c("dataset_type", "hb_name")) |>
  mutate(pop_rate_1000 = round(count / population * 1000, 2),
         tot_pop_rate_1000 = round(total / tot_population * 1000, 2)) |>
  save_as_parquet(path = paste0(ref_demo_dir, measure_label, "quarter_hb_simd"))

}

#Compile summary

compile_ref_demo_summary <- function(){
  
  #  load parquet files 
  p1 <- read_parquet(paste0(ref_demo_dir, "referrals_month_hb.parquet"))
  p2 <- read_parquet(paste0(ref_demo_dir, "referrals_month_hb_sex.parquet"))
  p3 <- read_parquet(paste0(ref_demo_dir, "referrals_month_hb_age.parquet"))
  p4 <- read_parquet(paste0(ref_demo_dir, "referrals_month_hb_simd.parquet"))
  
  p5 <- read_parquet(paste0(ref_demo_dir, "referrals_quarter_hb.parquet"))
  p6 <- read_parquet(paste0(ref_demo_dir, "referrals_quarter_hb_sex.parquet"))
  p7 <- read_parquet(paste0(ref_demo_dir, "referrals_quarter_hb_age.parquet"))
  p8 <- read_parquet(paste0(ref_demo_dir, "referrals_quarter_hb_simd.parquet"))

  # name tabs
  list_tabs <- list(
    
    month_hb = p1,
    month_sex = p2,
    month_age = p3,
    month_simd = p4,
    
    quart_hb = p5,
    quart_sex = p6,
    quart_age = p7,
    quart_simd = p8)
  
  # save output as excel doc
  filepath = paste0(shorewise_pub_measure_summaries_dir, "/referrals_demo_summary.xlsx")
  export(list_tabs, file = filepath)
  
  # format report
  wb <- loadWorkbook(filepath)
  
  for(i in 1:length(list_tabs)){
    setColWidths(wb, sheet = i, cols = 1:9, widths = "auto")
  }
  
  saveWorkbook(wb, filepath, overwrite =TRUE)
  
}
  