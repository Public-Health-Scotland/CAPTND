#####################################################.
##### For publication - care plan inclusion #####
#####################################################.

# Author: Luke Taylor
# Date: 2025-10-21

summarise_referrals_care_plan <- function(){
  
  ref_care_plan_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_care_plan/")
  dir.create(ref_care_plan_dir)
  measure_label <- "referrals_care_plan_"
  
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    filter(!!sym(referral_month_o) %in% date_range) |> 
    mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
           ref_quarter_ending = floor_date(ref_quarter, unit = "month"),
           care_plan_inc = as.numeric(care_plan_inc)) |> 
    arrange(dataset_type, ucpn) |>
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    fill("care_plan_inc", .direction = "downup") |>
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    add_sex_description() |> 
    tidy_age_group_order() |>
    remove_borders_int_refs()
  
  #set order of reason codes
  care_plan_order <- c('Yes', 'No', 'Not known', 'Data missing')
  
  ##Care Plan Inclusion##
  ##Month##
  
  df_mth_hb <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(care_plan_inc_o), referral_month) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(care_plan_inc_o), referral_month) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    ungroup() |>
    mutate(care_plan_inc = case_when(!!sym(care_plan_inc_o) == 1 ~ 'No',
                                     !!sym(care_plan_inc_o) == 2 ~ 'Yes',
                                     !!sym(care_plan_inc_o) == 99 ~ 'Not known',
                                     is.na(!!sym(care_plan_inc_o)) ~ 'Data missing')) |>
    select(referral_month, !!sym(dataset_type_o), !!sym(hb_name_o), !!sym(care_plan_inc_o), count) |>
    right_join(df_care_mth_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name", "care_plan_inc")) |>
    mutate(count = case_when(is.na(count) ~ 0,
                             TRUE ~ count)) |>
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
           care_plan_inc = factor(care_plan_inc, levels = care_plan_order)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |>
    mutate(total = sum(count),
           prop = round ( count / total * 100 , 2)) |> ungroup() |>
    save_as_parquet(path = paste0(ref_care_plan_dir, measure_label, "care_plan_mth_hb"))
  
}




