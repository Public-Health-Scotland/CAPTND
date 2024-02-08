
#################################.
### Monthly referrals by SIMD ###
#################################.

# Author: Charlie Smith
# Date: 2023-11-17

get_referrals_simd <- function(){
  
  # load reference data
  df_pop_simd <- import(paste0(reference_files_dir, 'pop_age_SIMD.xlsx')) |> 
    pivot_longer(cols = c(2:92), names_to = "Age", values_to = "Count") |> 
    mutate(Age = case_when(
      Age == "Age90plus" ~ "Age90",
      TRUE ~ Age),
      Age = as.numeric(str_replace(Age, "Age", "")))
  
  df_pop_simd_camhs <- df_pop_simd |> 
    filter(Age %in% c(0:18)) |> 
    group_by(SIMD2020_5) |> 
    summarise(Count = sum(Count)) |> 
    mutate(dataset_type = "CAMHS", .before = "SIMD2020_5")
  
  df_pop_simd_pt <- df_pop_simd |> 
    group_by(SIMD2020_5) |> 
    summarise(Count = sum(Count)) |> 
    mutate(dataset_type = "PT", .before = "SIMD2020_5")
  
  df_pop_simd <- bind_rows(df_pop_simd_camhs, df_pop_simd_pt) |> 
    rename(population_size = Count)
  
  
  # load captnd data
  df <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
    mutate(simd2020_quintile = as.character(simd2020_quintile)) |> 
    #filter(!is.na(!!sym(ref_acc_o))) %>% 
    select(all_of(data_keys), !!ref_acc_o, !!referral_month_o, !!simd_quintile_o) |>  
    distinct() |>  
    group_by(!!!syms(c(dataset_type_o, referral_month_o, simd_quintile_o))) |>  
    summarise(referrals = n(), .groups = 'drop') |> 
    left_join(df_pop_simd, by = c("dataset_type", "simd2020_quintile" = "SIMD2020_5")) |> 
    mutate(referral_rate =  round(referrals / population_size * 1000, digits = 1)) |> 
    
    save_as_parquet(path = paste0(data_working_safe, "refs_simd"))  
  
}
