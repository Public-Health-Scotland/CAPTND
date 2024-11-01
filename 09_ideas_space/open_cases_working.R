
month_end <- "2024-08-01"

source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

sub_month_end <- ymd(month_end)
sub_month_start <- ymd(month_end) - months(14)

month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
df_month_seq_start <- data.frame(sub_month_start = floor_date(month_seq, unit = "month")) # month_last_day

month_range <- seq.Date(from = sub_month_end-months(14), to = sub_month_end, by = "month")


summarise_open_cases <- function(){
  
  dir.create(open_dir)
  measure_label <- "open_cases_"
  
  # load data
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
  # single row per individual
  df_single_row <- df |>
    select(!!!syms(data_keys), !!sym(sex_reported_o), !!sym(age_group_o), !!sym(simd_quintile_o), 
           !!sym(rtt_eval_o), !!sym(referral_month_o),!!sym(case_closed_date_o), 
           !!sym(case_closed_month_o), !!sym(act_code_sent_date_o), !!sym(first_treat_app_o)) |>
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
  
  #tag first appt as online or in-person
  online_inperson_df <- df_single_row |>
    mutate(appt_type = case_when(!is.na(!!sym(act_code_sent_date_o)) &
                                   !!sym(act_code_sent_date_o) < !!sym(first_treat_app_o) ~ 'Online',
                              TRUE ~ 'In-Person'))
  
  
  # by month ----------------------------------------------------------------
  
  # by hb and month
  df_month_hb <- df_single_row |> 
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
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o))) |> 
    filter(sub_month_start == max(sub_month_start)) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb"))
  
  
  # by hb, month, and sex
  df_month_hb_sex <- df_single_row  |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(sex_reported_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    filter(sub_month_start %in% date_range) |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(sex_reported_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_sex")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, sex_reported_o))) |> 
    filter(sub_month_start == max(sub_month_start)) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o)) |>
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_sex"))
  
  
  # by hb, month, and age
  df_month_hb_age <- df_single_row  |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(age_group_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |>
    filter(sub_month_start %in% date_range) |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(age_group_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_age")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, age_group_o))) |> 
    filter(sub_month_start == max(sub_month_start)) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "age_group")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(age_group_o)) |>
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_age"))
  
  
  # by hb, month, and simd
  df_month_hb_simd <- df_single_row  |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(hb_name_o), #age_at_ref_rec, 
             !!sym(simd_quintile_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    filter(sub_month_start %in% date_range) |> 
    group_by(sub_month_start, !!sym(dataset_type_o), !!sym(simd_quintile_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |> 
    right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
    save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_simd")) |> 
    append_quarter_ending(date_col = "sub_month_start") |> 
    ungroup() |> 
    group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o, simd_quintile_o))) |> 
    filter(sub_month_start == max(sub_month_start)) |> # need last value per quarter only
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "simd2020_quintile")) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(simd_quintile_o)) |>
    save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_simd"))
  



# online + in-person comparison --------------------------------------------

# by hb and month
df_month_hb_appt_type <- online_inperson_df |> 
  group_by(sub_month_start, !!sym(dataset_type_o), !!sym(hb_name_o), appt_type) |> 
  summarise(count = n(), .groups = "drop") |>
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  filter(sub_month_start %in% date_range) |> 
  group_by(sub_month_start, !!sym(dataset_type_o), appt_type) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  right_join(df_month_ds_hb, by = c("sub_month_start" = "month", "dataset_type", "hb_name")) |> 
  save_as_parquet(path = paste0(open_dir, measure_label, "month_hb_appt_type")) |> 
  append_quarter_ending(date_col = "sub_month_start") |> 
  group_by(quarter_ending, !!!syms(c(dataset_type_o, hb_name_o)), appt_type) |> 
  filter(sub_month_start == max(sub_month_start)) |> # need last value per quarter only
  summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "appt_type")) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  save_as_parquet(path = paste0(open_dir, measure_label, "quarter_hb_appt_type"))

}