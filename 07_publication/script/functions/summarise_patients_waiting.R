
####################################################.
### Calculate monthly waits for patients waiting ###
####################################################.

# Author: Luke Taylor
# Date: 2024-09-24

# 1 Establish time frame--------------------------------------------------------
sub_month_end <- ymd(month_end)
sub_month_start <- ymd(month_end) - months(14)

month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
df_month_seq_end <- data.frame(sub_month_end = ceiling_date(month_seq, unit = "month")-1) # month_last_day

month_range <- seq.Date(from = sub_month_end-months(14), to = sub_month_end, by = "month")

# 2 Calculate patients waiting--------------------------------------------------
summarise_patients_waiting <- function(){
  
  dir.create(pat_waits_dir)
  measure_label <- "patients_wait_"
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    remove_borders_int_refs() |>
    #filter(!!sym(referral_month_o) <= month_end) |> # want total to latest month end
    select(!!!syms(c(header_date_o, file_id_o, dataset_type_o, hb_name_o, ucpn_o, 
                     patient_id_o, sex_reported_o,age_group_o, simd_quintile_o, 
                     ref_rec_date_o, ref_rej_date_o, app_date_o, first_treat_app_o, 
                     unav_date_start_o, unav_date_end_o, unav_days_no_o,
                     rtt_eval_o, case_closed_date_o, act_code_sent_date_o))) |>
    arrange(!!sym(header_date_o)) |> 
    group_by(across(all_of(data_keys))) |> 
    fill(!!sym(first_treat_app_o), .direction = "downup") |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_end) |>
    add_sex_description() |> 
    tidy_age_group_order()
    
  
  df_waits <- df_single_row |> 
    mutate(off_list_date = coalesce(!!sym(first_treat_app_o), !!sym(case_closed_date_o),
                                    !!sym(act_code_sent_date_o)),
           sub_month_start = floor_date(sub_month_end, unit = "month"),
           off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1), 
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1), 
           
           # add wait status
           wait_status = case_when(
             !is.na(ref_rej_date) & rej_month_end <= sub_month_end ~ "rejected",
             ref_rec_date <= sub_month_end & is.na(off_list_date) ~ "on list",
             off_list_month_end == sub_month_end ~ "tx Start",
             off_list_date > sub_month_end & ref_rec_date < sub_month_end ~ "on list",
             TRUE ~ NA_character_),
           
           # add wait time
           wait_days_unadj = ifelse(wait_status == "on list", round((sub_month_end-ref_rec_date), 1), NA_real_),
           wait_wks_unadj = ifelse(wait_status == "on list", round(wait_days_unadj/7, 1), NA_real_),
           
           # add adjusted waits - needs a bit of work
           #wait_days_adj = ifelse(wait_status == "on list", wait_days_unadj - unav_days_total, NA_real_),
           #wait_wks_adj = ifelse(wait_status == "on list", round(wait_days_adj/7, 1), NA_real_),
           
           # add rtt status
           wait_group_unadj = case_when(
             wait_status == "on list" & wait_wks_unadj >= 0 & wait_wks_unadj <= 18 ~ "wait_0_to_18_weeks",
             wait_status == "on list" & wait_wks_unadj > 18 & wait_wks_unadj <= 35 ~ "wait_19_to_35_weeks",
             wait_status == "on list" & wait_wks_unadj > 35 & wait_wks_unadj <= 52 ~ "wait_36_to_52_weeks",
             wait_status == "on list" & wait_wks_unadj > 52 ~ "over_52_weeks",
             TRUE ~ NA_character_),
           wait_group_unadj = factor(wait_group_unadj, levels = c("wait_0_to_18_weeks", "wait_19_to_35_weeks", 
                                                                  "wait_36_to_52_weeks", "over_52_weeks"))) |> 
    filter(!is.na(wait_group_unadj))
    
  
  # by month ----------------------------------------------------------------
  
  sub_month_start_o <- "sub_month_start"
  wait_group_unadj_o <- "wait_group_unadj"
  
  # by hb and month
  df_month_hb <- df_waits |> 
    filter(sub_month_start %in% date_range) |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o, wait_group_unadj_o))) |> 
    summarise(count = n()) |> 
    ungroup() |> 
    group_by(!!!syms(c(dataset_type_o, sub_month_start_o, wait_group_unadj_o))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop"))|> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o))|> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o))) |> 
    filter(!!sym(sub_month_start_o) == max(!!sym(sub_month_start_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb"))
  
  
  # by hb, month, and sex
  df_month_hb_sex <- df_waits |> 
    group_by(!!sym(sub_month_start_o), !!sym(wait_group_unadj_o), !!sym(dataset_type_o), 
             !!sym(hb_name_o),!!sym(sex_reported_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>  
    filter(!!sym(sub_month_start_o) %in% date_range) |> 
    group_by(!!sym(sub_month_start_o), !!sym(dataset_type_o), !!sym(sex_reported_o), !!sym(wait_group_unadj_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb_sex")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, sex_reported_o))) |> 
    filter(!!sym(sub_month_start_o) == max(!!sym(sub_month_start_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                       "sex_reported", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb_sex"))
  
  
  # by hb, month, and age
  df_month_hb_age <- df_waits |> 
    group_by(!!sym(sub_month_start_o), !!sym(dataset_type_o), !!sym(hb_name_o), 
             !!sym(age_group_o), !!sym(wait_group_unadj_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    filter(!!sym(sub_month_start_o) %in% date_range) |> 
    group_by(!!sym(sub_month_start_o), !!sym(dataset_type_o), !!sym(age_group_o), !!sym(wait_group_unadj_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb_age")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, age_group_o))) |> 
    filter(!!sym(sub_month_start_o) == max(!!sym(sub_month_start_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                       "age_group", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb_age"))
  
  
  # by hb, month, and simd
  df_month_hb_simd <- df_waits |> 
    group_by(!!sym(sub_month_start_o), !!sym(dataset_type_o), !!sym(hb_name_o), 
             !!sym(simd_quintile_o), !!sym(wait_group_unadj_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    filter(!!sym(sub_month_start_o) %in% date_range) |> 
    group_by(!!sym(sub_month_start_o), !!sym(dataset_type_o), !!sym(simd_quintile_o), !!sym(wait_group_unadj_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    filter(!(hb_name == "NHS 24")) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "month_hb_simd")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, simd_quintile_o))) |> 
    filter(!!sym(sub_month_start_o) == max(!!sym(sub_month_start_o))) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", 
                                       "simd2020_quintile", "wait_group_unadj")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "quarter_hb_simd"))
  
}