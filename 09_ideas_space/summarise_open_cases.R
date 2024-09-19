
############################.
### Calculate open cases ###
############################.

# Author: Charlie Smith
# Date: 2024-09-19


summarise_open_cases <- function(){

  dir.create(open_dir)
  measure_label <- "open_cases_"
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    lazy_dt() |> 
    filter(!!sym(referral_month_o) %in% date_range & # apply date range filter
             !!sym(rtt_eval_o) == "seen - active") |> # the same as open cases?
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    add_sex_description() |> 
    tidy_age_group_order() |> 
    as.data.frame() 


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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "all_hb"))
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "all_hb_sex"))
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "all_hb_age"))
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "all_hb_simd"))
  
  
  
  
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    #add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb"))
  
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_sex")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_sex"))
  
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_age")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", #"age_at_ref_rec", 
                                       "age_group")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type","hb_name"#, "age_at_ref_rec"
    )) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_age"))
  
  
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
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_simd")) |> 
    
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_simd"))
  
}


