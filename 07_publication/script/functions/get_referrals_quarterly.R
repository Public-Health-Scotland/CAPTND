
##################################.
### Quarterly referrals by HB  ###
##################################.

# Author: Charlie Smith
# Date: 2023-11-17


get_referrals_quarterly <- function(){
  
  ref_quarter_ending_o <- "ref_quarter_ending"
  
  df_hb <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
      #filter(!is.na(!!sym(ref_acc_o))) %>% 
      select(all_of(data_keys),!!ref_acc_o, ref_quarter_ending_o) %>% 
      distinct() |>  
      group_by(!!!syms(c(dataset_type_o, hb_name_o, ref_quarter_ending_o))) |>  
      summarise(referrals = n(), .groups = 'drop')
  
  df_sco <- df_hb |> 
    group_by(!!!syms(c(dataset_type_o, ref_quarter_ending_o))) |> 
    summarise(referrals = sum(referrals, na.rm = TRUE)) |> 
    mutate(hb_name = "NHS Scotland")
    
  df_all <- bind_rows(df_hb, df_sco) |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_quarterly_hb"))  
  
}






