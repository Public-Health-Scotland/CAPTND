
######################################.
### NHS Scotland referrals monthly ###
######################################.

# Author: Charlie Smith
# Date: 2023-11-17

get_referrals_sex_age <- function(){
  
  df <- read_parquet(paste0('../../../data/publication_files/captnd_pub.parquet')) |> 
    group_by(dataset_type, sex_reported, age_at_ref_rec) |> 
    summarise(referrals = n_distinct(patient_id)) |> 
    ungroup() |> 
    mutate(sex = case_when(
      sex_reported == 1 ~ 'Male',
      sex_reported == 2 ~ 'Female',
      is.na(sex_reported) ~ "Not known",
      TRUE ~ "Other")) |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_sex_age"))  
  
}
