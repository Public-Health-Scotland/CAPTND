
######################################.
### NHS Scotland referrals monthly ###
######################################.

# Author: Charlie Smith
# Date: 2023-11-17

get_referrals_sex_age <- function(){
  
  df <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
    #filter(!is.na(!!sym(ref_acc_o))) %>% 
    select(all_of(data_keys),!!ref_acc_o, sex_reported_o, age_at_ref_rec_o) |>  
    distinct() |>  
    group_by(!!!syms(c(dataset_type_o, sex_reported_o, age_at_ref_rec_o))) |>  
    summarise(referrals = n(), .groups = 'drop') |> 
    mutate(sex = case_when(
      sex_reported == 1 ~ 'Male',
      sex_reported == 2 ~ 'Female',
      is.na(sex_reported) ~ "Not known",
      TRUE ~ "Other")) |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_sex_age"))  
  
  
  # get median ages
  df_med <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
    select(all_of(data_keys),!!ref_acc_o, sex_reported_o, age_at_ref_rec_o) |>  
    distinct() |>
    ungroup() |> 
    group_by(dataset_type, sex_reported) |> 
    mutate(med_age = median(age_at_ref_rec, na.rm = TRUE)) |> 
    arrange(dataset_type, sex_reported)
    
  
  
}
