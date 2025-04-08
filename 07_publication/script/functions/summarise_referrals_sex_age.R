
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
    filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
    lazy_dt() |> 
    group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
    as.data.frame() |> 
    add_sex_description() |> 
    tidy_age_group_order() |>
    remove_borders_int_refs()
  
  
  df_month_hb <- df_single_row |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(sex_reported_o),
             !!sym(age_at_ref_rec_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(dataset_type_o), !!sym(sex_reported_o), !!sym(age_at_ref_rec_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    arrange(!!dataset_type_o, !!hb_name_o) |> 
    save_as_parquet(path = paste0(ref_dir, measure_label))
  
  #latest quarter for in-text summary stats
  df_last_qt_hb <- df_single_row |> 
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(hb_name_o),
             !!sym(sex_reported_o), !!sym(age_at_ref_rec_o)) |> 
    summarise(count = n(), .groups = "drop") |>
    group_by(!!sym(referral_month_o), !!sym(dataset_type_o), !!sym(sex_reported_o),
             !!sym(age_at_ref_rec_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
        mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector)) |> 
    append_quarter_ending(date_col = "referral_month") |> 
    summarise_by_quarter(vec_group = c("quarter_ending", "dataset_type", "hb_name", "sex_reported", "age_at_ref_rec")) |> 
    add_proportion_ds_hb(vec_group = c("quarter_ending", "dataset_type", "hb_name")) |> 
    arrange(!!dataset_type_o, !!hb_name_o)  |> 
    save_as_parquet(path = paste0(ref_dir, measure_label, "_qt"))
 
  
}