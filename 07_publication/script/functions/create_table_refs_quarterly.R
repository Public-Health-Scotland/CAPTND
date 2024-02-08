
#############################################.
### Create quarterly referral table by HB ###
#############################################.

# Author: Charlie Smith
# Date: 2023-11-20

create_table_referrals_quarterly_hb <- function(dataset_choice = c("CAMHS", "PT")){
  
  # load and wrangle data
  df_refs_quarterly <- read_parquet(file = paste0(data_working_safe, "refs_quarterly_hb.parquet")) |> 
    filter(dataset_type == dataset_choice) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(hb_name) |> 
    mutate(ref_quarter_ending = format(ref_quarter_ending, "%b %Y"), 
           referrals = format(referrals, big.mark=",")) |> 
    pivot_wider(names_from = ref_quarter_ending, values_from = referrals) |> 
    mutate_all(~replace(., is.na(.), ".."))
    
  
  # save df
  save_as_parquet(df_refs_quarterly, path = paste0(data_working_safe, "table_refs_quarterly_", dataset_choice))
  
  
}


