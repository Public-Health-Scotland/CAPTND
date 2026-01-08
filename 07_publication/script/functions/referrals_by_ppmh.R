################################################.
### For publication - referrals by perinatal ###
################################################.

# Author: Bex Madden
# Date: 2024-11-28


summarise_referrals_ppmh <- function(){
  
ref_ppmh_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/")
dir.create(ref_ppmh_dir)
measure_label <- "referrals_ppmh_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  ungroup() |>  
  add_sex_description() |> 
  remove_borders_int_refs() |>
  mutate(preg_perinatal = case_when(preg_perinatal == 1 ~ "Not pregnant",
                                    preg_perinatal == 2 ~ "Currently pregnant",
                                    preg_perinatal == 3 ~ "Post natal (within 12 months)",
                                    preg_perinatal == 96 ~ "Not applicable",
                                    preg_perinatal == 99 ~ "Not known",
                                    TRUE ~ "Data missing")) 


#updated df
updated_sex_groups_df <- df_single_row |>
  mutate(sex = case_when(!!sym(sex_o) == 0 ~ 'Not known',
                         !!sym(sex_o) == 9 ~ 'Not specified',
                         !!sym(sex_o) == 1 ~ 'Male',
                         !!sym(sex_o) == 2 ~ 'Female',
                         is.na(!!sym(sex_o)) ~ 'Data missing')) |>
  mutate(sex_reported = case_when(is.na(sex_reported) ~ sex,
                                  TRUE ~ sex_reported))

# overall -----------------------------------------------------------------
# by hb
df_all_hb <- updated_sex_groups_df |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o)) |>
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "all_hb"))

# by hb and sex
df_all_hb_sex <- updated_sex_groups_df |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "all_hb_sex")) #maleness dealt with differently - some not applicable, some not pregnant, some left as NA - argument for combining all non-pregnancies?


# quarterly ----------------------------------------------------------------

#skeleton df
ppmh_df <- data.frame(preg_perinatal = c('Not pregnant','Currently pregnant', 'Not known', 
                                        'Post natal (within 12 months)', 'Not applicable', 'Data missing'))

df_ppmh_qt_ds_hb <- df_qt_ds_hb |>
  cross_join(ppmh_df) |>
  filter(dataset_type == 'PT')

# by hb
df_qt_hb <- updated_sex_groups_df |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "qt_hb"))

# by hb and sex
df_qt_hb_sex <- updated_sex_groups_df |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> ungroup() |>
  filter(dataset_type == "PT", sex_reported == 'Female') |> 
  select(-!!sym(sex_reported_o)) |>
  right_join(df_ppmh_qt_ds_hb, by = c("ref_quarter_ending" = "quarter_ending", "dataset_type", "hb_name", "preg_perinatal")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_quarter_ending) |>
  mutate(total = sum(count),
         prop = round(count / total * 100 , 2)) |> ungroup() |>
  arrange(!!sym(hb_name_o), ref_quarter_ending) |> 
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "qt_hb_sex")) 

# monthly ----------------------------------------------------------------

#skeleton df
df_ppmh_mth_ds_hb <- df_month_ds_hb |>
  cross_join(ppmh_df) |>
  filter(dataset_type == 'PT')

# by hb
df_mth_hb <- updated_sex_groups_df |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), !!sym(referral_month_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), !!sym(referral_month_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o), !!sym(referral_month_o)) |> 
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "mth_hb"))

# by hb and sex
df_mth_hb_sex <- updated_sex_groups_df |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o), !!sym(referral_month_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o), !!sym(referral_month_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> ungroup() |>
  filter(dataset_type == "PT", sex_reported == 'Female') |> 
  select(-!!sym(sex_reported_o)) |>
  right_join(df_ppmh_mth_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name", "preg_perinatal")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  arrange(!!sym(hb_name_o), !!sym(referral_month_o)) |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |>
  mutate(total = sum(count),
         prop = round(count / total * 100 , 2)) |> ungroup() |>
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "mth_hb_sex")) 


}

