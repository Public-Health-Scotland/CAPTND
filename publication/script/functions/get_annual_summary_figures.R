
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
    group_by(dataset_type, sex) |> 
    summarise(count = n_distinct(patient_id), .groups = "drop") |> 
    group_by(dataset_type) |> 
    dplyr::mutate(total = sum(count, na.rm = TRUE),
           prop = round(count / total * 100, 2))
  
  # age group
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
    group_by(dataset_type, age_group_spec) |> 
    summarise(count = n_distinct(patient_id), .groups = "drop") |> 
    group_by(dataset_type) |> 
    dplyr::mutate(total = sum(count, na.rm = TRUE),
                  prop = round(count / total * 100, 2))
  
  # simd
  df_simd <- df |> 
    group_by(dataset_type, simd2020_quintile) |> 
    summarise(count = n_distinct(patient_id), .groups = "drop") |> 
    group_by(dataset_type) |> 
    dplyr::mutate(total = sum(count, na.rm = TRUE),
                  prop = round(count / total * 100, 2))
  
  
  # save
  tabs <- list(annual_sex = df_sex, 
               annual_age = df_age_group,
               annual_simd = df_simd)
  
  rio::export(tabs, paste0(data_working_safe, 'snapshot_12_months.xlsx'))
  
  
}