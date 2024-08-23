
###########################################################.
### Get referrals by sex and age for stem and leaf plot ###
###########################################################.

# Author: Charlie Smith
# Date: 2024-08-21

# NB this is a bit of a bodge due to last minute request


summarise_referrals_sex_age <- function(){
  
  measure_label <- "referrals_sex_age_hb"
  
  # single row per individual
  df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
    filter(referral_month %in% date_range) |> # apply date range filter
    group_by(dataset_type, hb_name, ucpn, patient_id) |> 
    slice(1) |> 
    ungroup() |> 
    add_sex_description() |> 
    tidy_age_group_order()
  
  
  df_month_hb <- df_single_row |> 
    group_by(dataset_type, hb_name, sex_reported, age_at_ref_rec) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(dataset_type, sex_reported, age_at_ref_rec) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    arrange(dataset_type, hb_name) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label))
  
  #latest quarter for in-text summary stats
  df_last_qt_hb <- df_single_row |> 
    group_by(referral_month, dataset_type, hb_name, sex_reported, age_at_ref_rec) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(referral_month, dataset_type, sex_reported, age_at_ref_rec) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
        mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported", "age_at_ref_rec")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(dataset_type, hb_name)  |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "_qt"))
 
  
}