
###############################.
### Publication - referrals ###
###############################.

# Author: Charlie Smith
# Date: 2024-07-16

summarise_referrals <- function(df){
  
  # create for for saving output files in
  #ref_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/")
  dir.create(ref_dir)
  measure_label <- "referrals_"
  
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    filter(referral_month %in% date_range) |> # apply date range filter
    group_by(dataset_type, hb_name, ucpn, patient_id) |> 
    slice(1) |> 
    ungroup() |> 
    add_sex_description() |> 
    tidy_age_group_order()
  
  
  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df_single_row |> 
    group_by(dataset_type, hb_name) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    #add_proportion_ds_hb() |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb"))
  
  # by sex
  df_all_sex <- df_single_row |> 
    group_by(dataset_type, hb_name, sex_reported) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, sex_reported) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb_sex"))
  
  # by age
  df_all_age <- df_single_row |> 
    group_by(dataset_type, hb_name, age_group) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, #age_at_ref_rec, 
             age_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |>
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |>  
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb_age"))
  
  # by simd
  df_all_simd <- df_single_row |> 
    group_by(dataset_type, hb_name, simd2020_quintile) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, simd2020_quintile) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb_simd"))
  
  
  
  
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_single_row |> 
    group_by(referral_month, dataset_type, hb_name) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(referral_month, dataset_type) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    #add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    #add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb"))
  
  
  # by hb, month, and sex
  df_month_hb_sex <- df_single_row |> 
    group_by(referral_month, dataset_type, hb_name, sex_reported) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(referral_month, dataset_type, sex_reported) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb_sex")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb_sex"))
  
  
  # by hb, month, and age
  df_month_hb_age <- df_single_row |> 
    group_by(referral_month, dataset_type, hb_name, #age_at_ref_rec, 
             age_group) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(referral_month, dataset_type, #age_at_ref_rec, 
             age_group) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name"#,"age_at_ref_rec"
    )) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb_age")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", #"age_at_ref_rec", 
                                       "age_group")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type","hb_name"#, "age_at_ref_rec"
    )) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb_age"))
  
  
  # by hb, month, and simd
  df_month_hb_simd <- df_single_row |> 
    group_by(referral_month, dataset_type, hb_name, simd2020_quintile) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(referral_month, dataset_type, simd2020_quintile) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    arrange(dataset_type, hb_name) |>  
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb_simd")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb_simd"))
  
}
