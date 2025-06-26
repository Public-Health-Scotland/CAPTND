####################################################.
### Calculate monthly waits for patients waiting ###
####################################################.

# Author: Charlie Smith & Bex Madden
# Date: 2024-03-07
# Edited: Luke Taylor 13/12/2024

#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

# NB this is to calculate unadjusted waits for those waiting to start treatment

calculate_pats_waiting_monthly <- function(df){
  
  source('04_check_modify/add_new_return_apps.R')
  
  sub_month_end <- ymd(month_end)
  sub_month_start <- ymd(month_end) - months(14)
  
  month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
  df_month_seq_end <- data.frame(sub_month_end = ceiling_date(month_seq, unit = "month")-1) # month_last_day
  
  month_range <- seq.Date(from = sub_month_end - months(14), to = sub_month_end, by = "month")
  
  #load df
  # df_comp <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  #   #check_multi_discharge_dates() |>
  #   filter(case_closed_opti >= ref_rec_date_opti | is.na(case_closed_opti), #remove cases with discharge date after ref recieved date
  #          app_date >= ref_rec_date_opti | is.na(app_date)) |> #remove appts before ref received date
  #   add_new_return_apps()
  
  
  # single row per individual
  df_single_row <- df |>
    remove_borders_int_refs() |>
    #filter(!!sym(referral_month_o) <= month_end) |> # want total to latest month end
    select(!!!syms(c(header_date_o, file_id_o, dataset_type_o, hb_name_o, ucpn_o, 
                     patient_id_o, sex_reported_o,age_group_o, simd_quintile_o, 
                     ref_rec_date_o, ref_rej_date_o, app_date_o, first_treat_app_o, 
                     unav_date_start_o, unav_date_end_o, unav_days_no_o,
                     rtt_eval_o, act_code_sent_date_o, ref_rec_date_opti_o, case_closed_date_o)),
           case_closed_opti, ref_acc_opti) |>
    arrange(!!sym(header_date_o)) |> 
    group_by(across(all_of(data_keys))) |> 
    fill(!!sym(first_treat_app_o), .direction = "downup") |> 
    fill(!!sym(act_code_sent_date_o), .direction = "downup") |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_end) |>
    add_sex_description() |> 
    tidy_age_group_order()
  
  
  df_waits <- df_single_row |> 
    filter(ref_acc_opti %in% c(1,3), #only keep accepted or pending referrals
           is.na(!!sym(ref_rej_date_o)),
           !!sym(case_closed_date_o) >= !!sym(ref_rec_date_opti_o) | is.na(!!sym(case_closed_date_o))) |> #remove referrals with case closed date before ref_rec_date
    group_by(!!!syms(data_keys)) |>
    mutate(off_list_date = case_when(any(!is.na(!!sym(act_code_sent_date_o)) & !!sym(act_code_sent_date_o) < !!sym(first_treat_app_o)) |
                                       any(!is.na(act_code_sent_date_o)) & is.na(!!sym(first_treat_app_o)) ~ act_code_sent_date,
                                     !is.na(!!sym(first_treat_app_o)) ~ first_treat_app, 
                                     TRUE ~ NA_Date_)) |>
    
    mutate(off_list_date = case_when(is.na(off_list_date) & is.na(!!sym(case_closed_date_o)) ~ NA_Date_,
                                     is.na(off_list_date) & !is.na(!!sym(case_closed_date_o)) ~ case_closed_date,
                                     TRUE ~ off_list_date)) |>
    ungroup() |>
    
    mutate(sub_month_start = floor_date(sub_month_end, unit = "month"),
           off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1), 
           rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1), 
           
           # add wait status
           wait_status = case_when(
             !is.na(ref_rej_date) & rej_month_end <= sub_month_end ~ "rejected",
             ref_rec_date <= sub_month_end & is.na(off_list_date) ~ "on list",
             off_list_month_end == sub_month_end ~ "tx Start",
             off_list_date >= sub_month_end & ref_rec_date <= sub_month_end ~ "on list",
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
  
  
  # table
  
  sub_month_start_o <- "sub_month_start"
  wait_group_unadj_o <- "wait_group_unadj"
  
  # by hb and month
  table_wait_summary <- df_waits |> 
    filter(sub_month_start %in% month_range) |> 
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
    group_by(!!!syms(c(dataset_type_o, hb_name_o, sub_month_start_o))) |> 
    mutate(waiting_total = sum(count, na.rm = TRUE),
           waiting_prop = round(count / waiting_total *100, 1))
  
  # save df
  dir.create(paste0(patients_waiting_dir,"/by_month"))
  save_as_parquet(table_wait_summary, paste0(patients_waiting_dir,'/by_month/monthly_waits_patients_waiting_hb'))
  
  write_csv_arrow(table_wait_summary, paste0(patients_waiting_dir,'/nPatients_waiting_subSource_monthly.csv'))
  
}



