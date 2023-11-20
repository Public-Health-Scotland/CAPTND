
##################################.
### Quarterly referrals by HB  ###
##################################.

# Author: Charlie Smith
# Date: 2023-11-17


get_referrals_quarterly <- function(){
  
  df_hb <- read_parquet(paste0('../../../data/publication_files/captnd_pub.parquet')) |> 
    group_by(dataset_type, hb_name, ref_quarter_ending) |> 
    summarise(referrals = n_distinct(patient_id)) |> 
    ungroup()
  
  df_sco <- df_hb |> 
    group_by(dataset_type, ref_quarter_ending) |> 
    summarise(referrals = sum(referrals, na.rm = TRUE)) |> 
    mutate(hb_name = "NHS Scotland")
    
  df_all <- bind_rows(df_hb, df_sco) |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_quarterly_hb"))  
  
}






