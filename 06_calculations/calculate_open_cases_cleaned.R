#####################################.
## Calculates open cases - Updated ##.
#####################################.

# Author: Luke Taylor
# Date: 2024-11-01

summarise_open_cases <- function(){
  
  sub_month_end <- ymd(month_end)
  sub_month_start <- ymd(month_end) - months(14)
  
  month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
  df_month_seq_start <- data.frame(sub_month_start = floor_date(month_seq, unit = "month")) 
  
  dir.create(open_dir)
  measure_label <- "open_cases_"
  
  # load data
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  # single row per individual
  df_single_row <- df |>
    select(!!!syms(data_keys), !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), 
           !!sym(rtt_eval_o), !!sym(referral_month_o),!!sym(case_closed_date_o), !!sym(app_date_o),
           !!sym(ref_rec_date_o), !!sym(case_closed_month_o), !!sym(act_code_sent_date_o), !!sym(first_treat_app_o)) |>
    filter(!!sym(referral_month_o) <= month_end) |>
    
    #filter for patients with treatment start date
    filter(!is.na(!!sym(first_treat_app_o)) | !is.na(!!sym(act_code_sent_date_o))) |> #flag patients with first treat app or act code sent date
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    cross_join(df_month_seq_start) |>
    
    #filter for patients without case closed date, or case closed within last 15 months
    mutate(first_treat_app = case_when(is.na(!!sym(first_treat_app_o)) ~ !!sym(act_code_sent_date_o),
                                       TRUE ~ !!sym(first_treat_app_o))) |> #when first treat appt date missing complete using act_code_sent_date
    mutate(first_treat_app_month = floor_date(!!sym(first_treat_app_o), unit = "month")) |> #create first treatment appt month column
    filter(sub_month_start >= first_treat_app_month) |>
    filter(is.na(!!sym(case_closed_date_o)) | #flag patients without a case closed date
             sub_month_start <= !!sym(case_closed_date_o)) |> #flag rows with a sub_month less than or equal to case_closed_date
    add_sex_description() |> 
    tidy_age_group_order() |>
    as.data.frame()
  
  #remove patients with no contact for more than 18 months since last appointment
  latest_contact <- df |>
    select(!!!syms(data_keys), !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), 
           !!sym(rtt_eval_o), !!sym(referral_month_o),!!sym(case_closed_date_o), !!sym(app_date_o),
           !!sym(ref_rec_date_o), !!sym(case_closed_month_o), !!sym(act_code_sent_date_o), !!sym(first_treat_app_o)) |>
    filter(!!sym(referral_month_o) <= month_end) |>
    group_by(across(all_of(data_keys))) |>
    arrange(app_date, .by_group = TRUE) |>
    slice(which.max(app_date)) |>
    ungroup() |>
    mutate(days_since_last_contact = as.numeric(Sys.Date() - app_date)) |>
    filter(is.na(!!sym(case_closed_date_o)) & days_since_last_contact >= 548) |> #18 months no contact and no case closed date
    select(!!!syms(data_keys)) |>
    mutate(flag = 1)
  
  #remove patients with no contact in last 18 months and digital referrals for specific hbs
  df_adjusted <- df_single_row |>
    left_join(latest_contact, by = c('dataset_type', 'hb_name', 'ucpn', 'patient_id')) |>
    filter(is.na(flag)) |>
    #remove online referrals for following HBs (never provide discharge date)
    mutate(flag = if_else(
      hb_name %in% c('NHS Grampian', 'NHS Highland','NHS Lanarkshire', 'NHS Tayside',
                     'NHS Shetland', 'NHS Greater Glasgow and Clyde', 'NHS Dumfries and Galloway') 
      & !is.na(act_code_sent_date), 1, 0)) |>
    filter(flag == 0) |>
    select(-flag)
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_adjusted |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    filter(sub_month_start %in% date_range) |> 
    group_by(sub_month_start, !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "cleaned_month_hb")) 
  
}