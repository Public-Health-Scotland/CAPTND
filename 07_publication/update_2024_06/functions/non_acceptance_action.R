
##############################################.
### Publication - non-acceptance by action ###
##############################################.

# Author: Charlie Smith
# Date: 2024-06-04

summarise_non_acceptance_action <- function(df){
  
  # create for for saving output files in
  #non_acc_action_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance/")
  dir.create(non_acc_action_dir)
  measure_label <- "non_acceptance_action_"
  
  # get non-acceptance reasons
  lookup_rej_action <- import('../../../data/captnd_codes_lookup.xlsx', which = 'Rej_Action') |> 
    rename(ref_rej_act = REJ_ACTIONS,
           ref_rej_act_desc = Rej_Action) |> 
    select(1:2)
  
  # get data to work on
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    filter(!!sym(referral_month_o) %in% date_range & # apply date range filter
             !!sym(ref_acc_last_reported_o) == 2) |> # not-accepted referrals only
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    left_join(lookup_rej_action, by = "ref_rej_act") |> 
    add_sex_description() |> 
    tidy_age_group_order()
  

  # overall -----------------------------------------------------------------
  
  # by hb
  df_all_hb <- df |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), ref_rej_act_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), ref_rej_act_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    add_proportion_ds_hb() |> 
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "all_hb"))
    
  # by sex
  df_all_hb_sex <- df |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o), ref_rej_act_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o), ref_rej_act_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name", "sex_reported")) |> 
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "all_hb_sex"))
  
  # by age
  df_all_hb_age <- df |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_rej_act_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_rej_act_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name"#, "age_at_ref_rec"
                                       )) |> 
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "all_hb_age"))
  
  # by SIMD
  df_all_hb_simd <- df |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o), ref_rej_act_desc) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(simd_quintile_o), ref_rej_act_desc) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    add_proportion_ds_hb(vec_group = c("dataset_type", "hb_name", "simd2020_quintile")) |> 
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "all_hb_simd"))
  
  
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), ref_rej_act_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), dataset_type, ref_rej_act_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "month_hb")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "ref_rej_act_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |>
    arrange(!!dataset_type_o, !!hb_name_o) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "quarter_hb"))
  
  
  # by month/quarter, hb, and sex
  df_month_hb_sex <- df |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(sex_reported_o), ref_rej_act_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), dataset_type, !!sym(sex_reported_o), ref_rej_act_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "sex_reported", "hb_name")) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "month_hb_sex")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported", "ref_rej_act_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |>
    arrange(!!dataset_type_o, !!hb_name_o) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "quarter_hb_sex"))
  
  
  # by month/quarter, hb, and age
  df_month_hb_age <- df |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o), ref_rej_act_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), dataset_type, #age_at_ref_rec, 
             !!sym(age_group_o), ref_rej_act_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", #"age_at_ref_rec", 
                                       "hb_name")) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "month_hb_age")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", #"age_at_ref_rec", 
                                       "age_group", "ref_rej_act_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name"#, "age_at_ref_rec"
                                       )) |>
    arrange(!!dataset_type_o, !!hb_name_o) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "quarter_hb_age"))
  
  
  # by month/quarter, hb, and simd
  df_month_hb_simd <- df |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(simd_quintile_o), ref_rej_act_desc) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), dataset_type, !!sym(simd_quintile_o), ref_rej_act_desc) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |>
    full_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |>
    add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "simd2020_quintile", "hb_name")) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "month_hb_simd")) |>
    
    append_quarter_ending(date_col = "referral_month") |>
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name",
                                       "simd2020_quintile", "ref_rej_act_desc")) |>
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name",
                                       "simd2020_quintile")) |>
    arrange(!!dataset_type_o, !!hb_name_o) |>
    save_as_parquet(path = paste0(non_acc_action_dir, measure_label, "quarter_hb_simd"))
  
  
}






