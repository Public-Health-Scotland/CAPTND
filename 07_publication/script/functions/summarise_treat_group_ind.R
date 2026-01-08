
########################################.
### MMI - Treatment group/individual ###
########################################.

# Author: Luke Taylor
# Date: 2026-01-06

summarise_treat_group_ind <- function(df){
  
  # create for for saving output files in
  group_ind_dir <- paste0(shorewise_pub_data_dir, "/treat_group_ind/")
  dir.create(group_ind_dir)
  measure_label <- "treat_group_ind_"
  
  #### SETUP #####
  
  # load data
  df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 
  
  # set constants
  most_recent_month_in_data <- get_lastest_month_end(df) 
  
  month_end <- floor_date(most_recent_month_in_data, unit = "month")
  month_start <- ymd(month_end) - months(14)
  date_range <- seq.Date(from = month_start, to = month_end, by = "month")
  
  # select vars
  demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")
  
  # single row per individual
  create_treat_group_ind_df <- function(treat_group_ind = c('treat_group_or_ind_1', 'treat_group_or_ind_2', 'treat_group_or_ind_3')){
    
    df_treat_group_ind <- df |>
      remove_borders_int_refs() |>
      mutate(treat_start_date = case_when(is.na(treat_start_date) ~ header_date,
                                          TRUE ~ treat_start_date), #use header date if treat_start_date is missing
             treat_month = floor_date(treat_start_date, unit = "month"),
             treat_quarter = ceiling_date(treat_month, unit = "quarter") - 1,
             treat_quarter_ending = floor_date(treat_quarter, unit = "month")) |>
      filter(treat_start_date %in% date_range, #using header date rather than ref month
             !is.na(.data[[treat_group_ind]])) |>
      select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, all_of(treat_group_ind), treat_month, 
             treat_quarter_ending, treat_start_date, header_date) |>
      arrange(ucpn, .data[[treat_group_ind]], treat_start_date, header_date) |>
      group_by(!!!syms(data_keys), .data[[treat_group_ind]]) |>
      slice_head(n = 1) |>
      ungroup()
    
  }
  
  df_treat_group_ind_1 <- create_treat_group_ind_df('treat_group_or_ind_1')
  df_treat_group_ind_2 <- create_treat_group_ind_df('treat_group_or_ind_2')
  df_treat_group_ind_3 <- create_treat_group_ind_df('treat_group_or_ind_3')
  
  
  ####### QUARTERLY ########
  group_ind_df <- data.frame(treat_group_or_ind = c('Individual', 'Group', 'Not known'))
  
  df_group_ind_qt_ds_hb <- df_qt_ds_hb |>
    cross_join(group_ind_df) 
  
  #Treatment group 1
  treat_group_ind_1_qt <- df_treat_group_ind_1 |>
    filter(!is.na(treat_group_or_ind_1)) |>
    group_by(dataset_type, treat_group_or_ind_1, hb_name, treat_quarter_ending) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, treat_group_or_ind_1, treat_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(treat_group_or_ind_1 = case_when(treat_group_or_ind_1 == 1 ~ 'Individual',
                                          treat_group_or_ind_1 == 2 ~ 'Group',
                                          TRUE ~ 'Not known'),
           hb_name = factor(hb_name, levels = level_order_hb)) |>
    right_join(df_group_ind_qt_ds_hb, by = c("treat_quarter_ending" = "quarter_ending", "dataset_type", "hb_name", "treat_group_or_ind_1" = "treat_group_or_ind")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count),
           level = 'Primary') |>
    rename(treat_group_or_ind = treat_group_or_ind_1) |>
    arrange(dataset_type, hb_name, treat_quarter_ending) 
  
  #Reason for treatment 2
  treat_group_ind_2_qt <- df_treat_group_ind_2 |>
    filter(!is.na(treat_group_or_ind_2)) |>
    group_by(dataset_type, treat_group_or_ind_2, hb_name, treat_quarter_ending) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, treat_group_or_ind_2, treat_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(treat_group_or_ind_2 = case_when(treat_group_or_ind_2 == 1 ~ 'Individual',
                                            treat_group_or_ind_2 == 2 ~ 'Group',
                                            TRUE ~ 'Not known'),
           hb_name = factor(hb_name, levels = level_order_hb)) |>
    right_join(df_group_ind_qt_ds_hb, by = c("treat_quarter_ending" = "quarter_ending", "dataset_type", "hb_name", "treat_group_or_ind_2" = "treat_group_or_ind")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count),
           level = 'Secondary') |>
    rename(treat_group_or_ind = treat_group_or_ind_2) |>
    arrange(dataset_type, hb_name, treat_quarter_ending)
  
  #Reason for treatment 3
  treat_group_ind_3_qt <- df_treat_group_ind_3 |>
    filter(!is.na(treat_group_or_ind_3)) |>
    group_by(dataset_type, treat_group_or_ind_3, hb_name, treat_quarter_ending) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, treat_group_or_ind_3, treat_quarter_ending) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(treat_group_or_ind_3 = case_when(treat_group_or_ind_3 == 1 ~ 'Individual',
                                            treat_group_or_ind_3 == 2 ~ 'Group',
                                            TRUE ~ 'Not known'),
           hb_name = factor(hb_name, levels = level_order_hb)) |>
    right_join(df_group_ind_qt_ds_hb, by = c("treat_quarter_ending" = "quarter_ending", "dataset_type", "hb_name", "treat_group_or_ind_3" = "treat_group_or_ind")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count),
           level = 'Tertiary') |>
    rename(treat_group_or_ind = treat_group_or_ind_3) |>
    arrange(dataset_type, hb_name, treat_quarter_ending)

  
  treat_group_ind_qt  <- rbind(treat_group_ind_1_qt , treat_group_ind_2_qt , treat_group_ind_3_qt) |>
    ungroup() |>
    arrange(dataset_type, hb_name, treat_quarter_ending) |>
    save_as_parquet(paste0(group_ind_dir, measure_label, "qt"))
  
  
  ####### MONTHLY ########
  
  df_group_ind_mth_ds_hb <- df_month_ds_hb |>
    cross_join(group_ind_df) 
  
  #Treatment group 1
  treat_group_ind_1_mth <- df_treat_group_ind_1 |>
    filter(!is.na(treat_group_or_ind_1)) |>
    group_by(dataset_type, treat_group_or_ind_1, hb_name, treat_month) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, treat_group_or_ind_1, treat_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(treat_group_or_ind_1 = case_when(treat_group_or_ind_1 == 1 ~ 'Individual',
                                            treat_group_or_ind_1 == 2 ~ 'Group',
                                            TRUE ~ 'Not known'),
           hb_name = factor(hb_name, levels = level_order_hb)) |>
    right_join(df_group_ind_mth_ds_hb, by = c("treat_month" = "month", "dataset_type", "hb_name", "treat_group_or_ind_1" = "treat_group_or_ind")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count),
           level = 'Primary') |>
    rename(treat_group_or_ind = treat_group_or_ind_1) |>
    arrange(dataset_type, hb_name, treat_month) 
  
  #Reason for treatment 2
  treat_group_ind_2_mth <- df_treat_group_ind_2 |>
    filter(!is.na(treat_group_or_ind_2)) |>
    group_by(dataset_type, treat_group_or_ind_2, hb_name, treat_month) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, treat_group_or_ind_2, treat_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(treat_group_or_ind_2 = case_when(treat_group_or_ind_2 == 1 ~ 'Individual',
                                            treat_group_or_ind_2 == 2 ~ 'Group',
                                            TRUE ~ 'Not known'),
           hb_name = factor(hb_name, levels = level_order_hb)) |>
    right_join(df_group_ind_mth_ds_hb, by = c("treat_month" = "month", "dataset_type", "hb_name", "treat_group_or_ind_2" = "treat_group_or_ind")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count),
           level = 'Secondary') |>
    rename(treat_group_or_ind = treat_group_or_ind_2) |>
    arrange(dataset_type, hb_name, treat_month)
  
  #Reason for treatment 3
  treat_group_ind_3_mth <- df_treat_group_ind_3 |>
    filter(!is.na(treat_group_or_ind_3)) |>
    group_by(dataset_type, treat_group_or_ind_3, hb_name, treat_month) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, treat_group_or_ind_3, treat_month) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(treat_group_or_ind_3 = case_when(treat_group_or_ind_3 == 1 ~ 'Individual',
                                            treat_group_or_ind_3 == 2 ~ 'Group',
                                            TRUE ~ 'Not known'),
           hb_name = factor(hb_name, levels = level_order_hb)) |>
    right_join(df_group_ind_mth_ds_hb, by = c("treat_month" = "month", "dataset_type", "hb_name", "treat_group_or_ind_3" = "treat_group_or_ind")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count),
           level = 'Tertiary') |>
    rename(treat_group_or_ind = treat_group_or_ind_3) |>
    arrange(dataset_type, hb_name, treat_month)
  
  treat_group_ind_mth  <- rbind(treat_group_ind_1_mth , treat_group_ind_2_mth , treat_group_ind_3_mth) |>
    ungroup() |>
    arrange(dataset_type, hb_name, treat_month) |>
    save_as_parquet(paste0(group_ind_dir, measure_label, "mth"))
  
  
}



