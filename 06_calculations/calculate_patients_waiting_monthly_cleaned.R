
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
  
  #load dataframe
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
    select(!!!syms(c(header_date_o, file_id_o, dataset_type_o, hb_name_o, ucpn_o, 
                     patient_id_o, sex_reported_o,age_group_o, simd_quintile_o, 
                     ref_rec_date_o, ref_rej_date_o, app_date_o, first_treat_app_o, 
                     unav_date_start_o, unav_date_end_o, unav_days_no_o,
                     rtt_eval_o, case_closed_date_o, act_code_sent_date_o))) |>
    arrange(!!sym(header_date_o)) |> 
    group_by(across(all_of(data_keys))) |> 
    fill(!!sym(first_treat_app_o), .direction = "downup") 
  
  
  #cases waiting greater than 18 months since last contact
  last_contact_df <- df |>
    group_by(across(all_of(data_keys))) |> 
    filter(is.na(!!sym(case_closed_date_o)) & is.na(!!sym(act_code_sent_date_o))
           & is.na(!!sym(first_treat_app_o))) |>
    slice(which.max(app_date)) |>
    ungroup() |>
    mutate(off_list_date = coalesce(!!sym(first_treat_app_o), !!sym(case_closed_date_o),
                                    !!sym(act_code_sent_date_o)),
           sub_month_start = floor_date(sub_month_end, unit = "month"),
           off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1),
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1)) |>
    mutate(wait_status = case_when(
      !is.na(ref_rej_date) & rej_month_end <= sub_month_end ~ "rejected",
      ref_rec_date <= sub_month_end & is.na(off_list_date) ~ "on list",
      off_list_month_end == sub_month_end ~ "tx Start",
      off_list_date > sub_month_end & ref_rec_date < sub_month_end ~ "on list",
      TRUE ~ NA_character_)) 
  
  last_contact <- last_contact_df |>
    filter(wait_status == 'on list') |>
    mutate(days_since_last_appt = as.numeric(sub_month_start - app_date)) |>
    mutate(days_since_ref = as.numeric(sub_month_start - ref_rec_date)) |>
    filter(days_since_last_appt >= 548) |> #18 months since last appt
    select(!!!syms(data_keys)) |>
    mutate(flag = 1)
  
  #cases with no contact waiting greater than 24 months
  no_contact_df <- df |>
    group_by(across(all_of(data_keys))) |> 
    filter(is.na(!!sym(case_closed_date_o)) & is.na(!!sym(act_code_sent_date_o))
           & is.na(!!sym(first_treat_app_o)) & is.na(!!sym(app_date_o))) |>
    slice(1) |>
    ungroup() |>
    mutate(off_list_date = coalesce(!!sym(first_treat_app_o), !!sym(case_closed_date_o),
                                    !!sym(act_code_sent_date_o)),
           sub_month_start = floor_date(sub_month_end, unit = "month"),
           off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1),
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1)) |>
    mutate(wait_status = case_when(
      !is.na(ref_rej_date) & rej_month_end <= sub_month_end ~ "rejected",
      ref_rec_date <= sub_month_end & is.na(off_list_date) ~ "on list",
      off_list_month_end == sub_month_end ~ "tx Start",
      off_list_date > sub_month_end & ref_rec_date < sub_month_end ~ "on list",
      TRUE ~ NA_character_)) 
  
  no_contact <- no_contact_df |>
    filter(wait_status == 'on list') |>
    mutate(days_since_ref = as.numeric(sub_month_start - ref_rec_date)) |>
    filter(days_since_ref >= 730) |> #24 months no contact
    select(!!!syms(data_keys)) |>
    mutate(flag = 1)
  
  #bind dfs together
  patients_to_remove <- rbind(last_contact, no_contact) |>
    distinct()
  
  #single row per individual for complete dataset
  df_single_row <- df |> 
    group_by(across(all_of(data_keys))) |>
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
  
  #join long waits
  
  df_cleaned_waits <- df_waits |>
    left_join(patients_to_remove, by = c('dataset_type', 'hb_name', 'ucpn', 'patient_id')) |>
    filter(is.na(flag))
  
  
  # by month ----------------------------------------------------------------
  
  sub_month_start_o <- "sub_month_start"
  wait_group_unadj_o <- "wait_group_unadj"
  
  # by hb and month
  df_month_hb <- df_cleaned_waits |> 
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
    save_as_parquet(path = paste0(pat_waits_dir, measure_label, "cleaned_month_hb")) 
  
}



