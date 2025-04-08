
####################################.
### Publication - non-acceptance ###
####################################.

# Author: Charlie Smith
# Date: 2024-05-31

summarise_non_acceptance <- function(df){
  
  # create for for saving output files in
  #non_acc_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance/")
  dir.create(non_acc_dir)
  measure_label <- "non_acceptance_summary_"

  
  # get referral source lookup table
  lookup_acc <- import('../../../data/captnd_codes_lookup.xlsx', which = 'Ref_Accepted') |> 
    rename(ref_acc_opti = Code,
           ref_acc_desc = Values) |> 
    select(1:2) |> 
    mutate(ref_acc_desc = if_else(ref_acc_desc == "Referral rejected", 
                                  "Referral not accepted", 
                                  ref_acc_desc))
  
  # get data to work on
  df_rej <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    mutate(ref_quarter = ceiling_date(!!sym(referral_month_o), unit = "quarter") - 1,
           ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
    left_join(lookup_acc, by = c("ref_acc_opti")) |> 
    add_sex_description() |> 
    tidy_age_group_order() |> 
    remove_borders_int_refs() |>
    mutate(ref_acc_desc = if_else(is.na(ref_acc_desc), "No information", ref_acc_desc))
    
   
  
  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df_rej |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_acc_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), ref_acc_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb() |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "all_hb"))
    
  
  # by sex
  df_all_hb_sex <- df_rej |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o), ref_acc_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o), ref_acc_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name", "sex_reported")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "all_hb_sex"))
  
  # by age
  df_all_hb_age <- df_rej |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_acc_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_acc_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name", #"age_at_ref_rec", 
                                       "age_group")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "all_hb_age"))
  
  # by simd
  df_all_hb_simd <- df_rej |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o), ref_acc_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(simd_quintile_o), ref_acc_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name", "simd2020_quintile")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "all_hb_simd"))
  
  
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_rej |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), ref_acc_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), ref_acc_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "month_hb")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "ref_acc_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "quarter_hb"))


  # by hb, month, and sex
  df_month_hb_sex <- df_rej |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o), ref_acc_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o), ref_acc_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name", "sex_reported")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "month_hb_sex")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported", "ref_acc_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "quarter_hb_sex"))

  
  # by hb, month, and age
  df_month_hb_age <- df_rej |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_acc_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_acc_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name"#, "age_at_ref_rec"
                                       )) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "month_hb_age")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", #"age_at_ref_rec", 
                                       "age_group", "ref_acc_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name"#, "age_at_ref_rec"
                                       )) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "quarter_hb_age"))

   
  # by hb, month, and simd
  df_month_hb_simd <- df_rej |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), 
             !!sym(simd_quintile_o), ref_acc_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(simd_quintile_o), 
             ref_acc_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name", "simd2020_quintile")) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "month_hb_simd")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile", "ref_acc_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |>
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(non_acc_dir, measure_label, "quarter_hb_simd"))
  
}






