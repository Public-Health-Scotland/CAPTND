
#####################################.
### NHS Scotland referrals monthly###
#####################################.

# Author: Charlie Smith
# Date: 2023-11-17


get_referrals_monthly_sco <- function(){
  
  df <- read_parquet(paste0('../../../data/publication_files/captnd_pub.parquet')) |> 
    group_by(dataset_type, ref_month) |> 
    summarise(referrals = n_distinct(patient_id)) |> 
    ungroup() |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_monthly_sco"))  
  
}

