
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
    filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    add_sex_description() |> 
    tidy_age_group_order()

  
  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    #add_proportion_ds_hb() |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb"))
  
  # by sex
  df_all_sex <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb_sex"))
  
  # by age
  df_all_age <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), #age_at_ref_rec, 
             !!sym(age_group_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |>  
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb_age"))
  
  # by simd
  df_all_simd <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "all_hb_simd"))
  
  
  
  
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_single_row |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    #add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    #add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb"))
  
  
  # by hb, month, and sex
  df_month_hb_sex <- df_single_row |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(sex_reported_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb_sex")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb_sex"))
  
  
  # by hb, month, and age
  df_month_hb_age <- df_single_row |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), #age_at_ref_rec, 
             !!sym(age_group_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name"#,"age_at_ref_rec"
    )) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb_age")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", #"age_at_ref_rec", 
                                       "age_group")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type","hb_name"#, "age_at_ref_rec"
    )) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb_age"))
  
  
  # by hb, month, and simd
  df_month_hb_simd <- df_single_row |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(simd_quintile_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "month_hb_simd")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "quarter_hb_simd"))
  
}
