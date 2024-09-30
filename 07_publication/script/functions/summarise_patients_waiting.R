
####################################################.
### Calculate monthly waits for patients waiting ###
####################################################.

# Author: Luke Taylor
# Date: 2024-09-24

# 1 Establish time frame--------------------------------------------------------
month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)

month_seq <- seq.Date(from = ymd(month_start), to = ymd(month_end), by = "month")
df_month_seq_end <- data.frame(month_end = ceiling_date(month_seq, unit = "month")-1) # month_last_day

month_range <- seq.Date(from = month_end-months(14), to = month_end, by = "month")

# 2 Calculate patients waiting--------------------------------------------------
summarise_patients_waiting <- function(){
  
  dir.create(pat_waits_dir)
  measure_label <- "patients_wait_"
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    #filter(!!sym(referral_month_o) <= month_end) |> # want total to latest month end
    arrange(!!sym(header_date_o)) |> 
    group_by(across(all_of(data_keys))) |> 
    fill(!!sym(first_treat_app_o), .direction = "downup") |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_end) |>
    add_sex_description() |> 
    tidy_age_group_order()
    
  
  df_waits <- df_single_row |> 
    select(-!!sym(referral_month_o)) |>
    mutate(off_list_date = coalesce(!!sym(first_treat_app_o), !!sym(case_closed_date_o)), 
           referral_month = floor_date(month_end, unit = "month"),
           off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1), 
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1), 
           
           # add wait status
           wait_status = case_when(
             !is.na(ref_rej_date) & rej_month_end <= month_end ~ "rejected",
             ref_rec_date <= month_end & is.na(off_list_date) ~ "on list",
             off_list_month_end == month_end ~ "tx Start",
             off_list_date > month_end & ref_rec_date < month_end ~ "on list",
             TRUE ~ NA_character_),
           
           # add wait time
           wait_days_unadj = ifelse(wait_status == "on list", round((month_end-ref_rec_date), 1), NA_real_),
           wait_wks_unadj = ifelse(wait_status == "on list", round(wait_days_unadj/7, 1), NA_real_),
           
           # add adjusted waits - needs a bit of work
           #wait_days_adj = ifelse(wait_status == "on list", wait_days_unadj - unav_days_total, NA_real_),
           #wait_wks_adj = ifelse(wait_status == "on list", round(wait_days_adj/7, 1), NA_real_),
           
           # add rtt status
           wait_group_unadj = case_when(
             wait_status == "on list" & wait_wks_unadj >= 0 & wait_wks_unadj <= 18 ~ "wait_0_to_18_weeks",
             wait_status == "on list" & wait_wks_unadj > 18 & wait_wks_unadj <= 52 ~ "wait_19_to_52_weeks",
             wait_status == "on list" & wait_wks_unadj > 52 ~ "over_52_weeks",
             TRUE ~ NA_character_),
           wait_group_unadj = factor(wait_group_unadj, levels = c("wait_0_to_18_weeks", "wait_19_to_52_weeks", 
                                                                  "over_52_weeks"))) |> 
    filter(!is.na(wait_group_unadj))
    
  
  # by month ----------------------------------------------------------------
  
  referral_month_o <- "referral_month"
  wait_group_unadj_o <- "wait_group_unadj"
  
  # by hb and month
  df_month_hb <- df_waits |> 
    filter(referral_month %in% date_range) |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, referral_month_o, wait_group_unadj_o))) |> 
    summarise(count = n()) |> 
    ungroup() |> 
    group_by(!!!syms(c(dataset_type_o, referral_month_o, wait_group_unadj_o))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop"))|> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o))|> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb"))
  
  
  # by hb, month, and sex
  df_month_hb_sex <- df_waits |> 
    group_by(!!sym(referral_month_o), !!sym(wait_group_unadj_o), !!sym(dataset_type_o), 
             !!sym(hb_name_o),!!sym(sex_reported_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>  
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o), !!sym(wait_group_unadj_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb_sex")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, sex_reported_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                       "sex_reported", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb_sex"))
  
  
  # by hb, month, and age
  df_month_hb_age <- df_waits |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), 
             !!sym(age_group_o), !!sym(wait_group_unadj_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(age_group_o), !!sym(wait_group_unadj_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb_age")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, age_group_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                       "age_group", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb_age"))
  
  
  # by hb, month, and simd
  df_month_hb_simd <- df_waits |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o), 
             !!sym(simd_quintile_o), !!sym(wait_group_unadj_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    filter(!!sym(referral_month_o) %in% date_range) |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(simd_quintile_o), !!sym(wait_group_unadj_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    right_join(df_month_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb_simd")) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, simd_quintile_o))) |> 
    filter(!!sym(referral_month_o) == max(!!sym(referral_month_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                       "simd2020_quintile", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb_simd"))
  
}