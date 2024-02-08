
##################################.
### Get annual summary figures ###
##################################.

# Author: Charlie Smith
# Date: 2024-01-23

# summary figures for previous 12 months of data (e.g. in 2023, X% of referrals were male)
# to be used in report narrative

get_annual_summary_figs <- function(){
  
  # load data
  df <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
    filter(referral_month %in% range_12_month)
  
  # sex
  df_sex <- df |> 
    # filter(!is.na(!!sym(ref_acc_o))) |> 
    select(all_of(data_keys), sex_reported_o) |> 
    distinct() |> 
    group_by(!!!syms(c(dataset_type_o, sex_reported_o))) |>  
    summarise(count = n(), .groups = 'drop') |> 
    group_by(dataset_type) |> 
    dplyr::mutate(total = sum(count, na.rm = TRUE),
           prop = round(count / total * 100, 2))
  
  # age group
  age_group_spec_o <- "age_group_spec"
  
  df_age_group <- df |> 
    mutate(age_group_spec = if_else(dataset_type == "CAMHS", 
       # camhs age groups
       case_when(
        age_at_ref_rec >= 0 & age_at_ref_rec <= 4 ~ "0 to 4",
        age_at_ref_rec >= 5 & age_at_ref_rec <= 9 ~ "5 to 9",
        age_at_ref_rec >= 10 & age_at_ref_rec <= 14 ~ "10 to 14",
        age_at_ref_rec >= 15 & age_at_ref_rec <= 19 ~ "15 to 19",
        age_at_ref_rec >= 20 ~ "aged 20+"),
       # pt age groups        
       case_when(
          age_at_ref_rec >= 0 & age_at_ref_rec < 18 ~ "under 18",
          age_at_ref_rec >= 18 & age_at_ref_rec <= 64 ~ "18 to 64",
          age_at_ref_rec >= 65 ~ "aged 65+")
      )) |> 
    select(all_of(data_keys), age_group_spec_o) |> 
    distinct() |> 
    group_by(!!!syms(c(dataset_type_o, age_group_spec_o))) |>  
    summarise(count = n(), .groups = 'drop') |> 
    group_by(dataset_type) |> 
    dplyr::mutate(total = sum(count, na.rm = TRUE),
                  prop = round(count / total * 100, 2))
  
  # simd
  df_simd <- df |> 
    select(all_of(data_keys), simd_quintile_o) |> 
    distinct() |> 
    group_by(!!!syms(c(dataset_type_o, simd_quintile_o))) |>  
    summarise(count = n(), .groups = 'drop') |> 
    group_by(dataset_type) |> 
    dplyr::mutate(total = sum(count, na.rm = TRUE),
                  prop = round(count / total * 100, 2))
  
  
  # save
  tabs <- list(annual_sex = df_sex, 
               annual_age = df_age_group,
               annual_simd = df_simd)
  
  rio::export(tabs, paste0(data_working_safe, 'snapshot_12_months.xlsx'))
  
  
}