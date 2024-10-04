
############################.
### Calculate open cases ###
############################.

# Author: Charlie Smith
# Date: 2024-09-19

# This is more fiddly than I anticipated. It makes sense to report monthly open cases 
# as cumulative sums

##comments
#row 29 - by filtering for those with 'seen - active' we would be excluding patients who were an open case
# within the last 15 months, but who have since had there case closed, as their rtt_eval will have been back 
#filled with 'case closed'. Might need to create a new column that identifies months in which a patient was
#an open case to avoid missing these in monthly counts.

#row 118 - currently count is grouped by referral_month. This should maybe be a floored first appointment referral
#date, just so that patients are not counted as open between referral month and first treatment appt?


summarise_open_cases <- function(){
  
  dir.create(open_dir)
  measure_label <- "open_cases_"
  open_cases_criteria <- c("seen - active", "seen - online - active")
  
  # load data
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  # single row per individual
  df_single_row <- df |> 
    lazy_dt() |> 
    filter(!!sym(referral_month_o) <= month_end & # want total to latest month end
             !!sym(rtt_eval_o) %in% open_cases_criteria) |> # the same as open cases?
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    add_sex_description() |> 
    tidy_age_group_order() |> 
    as.data.frame() 

  df_single_row_monthly <- df |> 
    lazy_dt() |> 
    filter(#!!sym(referral_month_o) %in% date_range & # want to apply range filter later
             !!sym(rtt_eval_o) %in% open_cases_criteria) |> # the same as open cases?
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
    #add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |> 
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
    #add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |>  
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
    #add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "all_hb_simd"))
  
  
  
  
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_single_row_monthly |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    mutate(count = cumsum(count)) |> 
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb"))
  
  
  # by hb, month, and sex
  df_month_hb_sex <- df_single_row_monthly |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(sex_reported_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    mutate(count = cumsum(count)) |> 
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_sex")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, sex_reported_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_sex"))
  
  
  # by hb, month, and age
  df_month_hb_age <- df_single_row_monthly |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    mutate(count = cumsum(count)) |> 
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(age_group_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_age")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, age_group_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "age_group")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |>
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_age"))
  
   
  
  
  # by hb, month, and simd
  df_month_hb_simd <- df_single_row_monthly |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(simd_quintile_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    mutate(count = cumsum(count)) |> 
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_simd")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, simd_quintile_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_simd"))

}


