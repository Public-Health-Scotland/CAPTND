#############################.
### Create WL extract csv ###
#############################.

# Author: Luke Taylor
# Date: 2025-06-16

# Will write a csv file with waiting list for most recent month, based on month_end

write_wl_extract <- function(HB, dataset_choice){

# 1 Establish time frame--------------------------------------------------------
sub_month_end <- ymd(month_end)
sub_month_start <- ymd(month_end) - months(14)

month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
df_month_seq_end <- data.frame(sub_month_end = ceiling_date(month_seq, unit = "month")-1) # month_last_day

month_range <- seq.Date(from = sub_month_end-months(14), to = sub_month_end, by = "month")

# 2 Calculate patients waiting--------------------------------------------------

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
                     rtt_eval_o, case_closed_date_o, act_code_sent_date_o, ref_source_o)),
           ref_acc_opti) |>
    arrange(!!sym(header_date_o)) |> 
    group_by(across(all_of(data_keys))) |> 
    fill(!!sym(first_treat_app_o), .direction = "downup") |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_end) |>
    add_sex_description() |> 
    tidy_age_group_order()
  
  
  df_waits <- df_single_row |> 
    filter(ref_acc_opti %in% c(1,3)) |>
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
  
# 3 Write csv for latest month--------------------------------------------------  
  df_month_hb <- df_waits |> 
    filter(sub_month_start == month_end &
             hb_name == HB &
             dataset_type == dataset_choice &
             is.na(off_list_date)) |> 
    select(!!!syms(data_keys), ref_rec_date, sub_month_end, wait_status) |>
    write_csv(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/wl_extracts/", 
                     HB, "_" , dataset_choice, "_", month_end, "_", "wl_extract.csv"))

}

