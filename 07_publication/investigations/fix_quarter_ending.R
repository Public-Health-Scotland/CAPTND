
################################.
### Fix quarter ending dates ###
################################.

# Author: Charlie Smith
# Date: 2024-02-27

#df = basic_camhs_quart_hb

fix_quarter_ending <- function(df){
  
  df_fixed <- df |> 
    mutate(quarter_ending = paste0("01-", quarter_ending), 
           quarter_ending = str_replace(quarter_ending, " ", "-"), 
           quarter_ending = dmy(quarter_ending),
           referrals = str_replace(referrals, ",", ""),
           referrals = as.numeric(referrals),
           HB = as.character(HB)) |> 
    rename(hb_name = HB, 
           ref_quarter_ending = quarter_ending)
  
  return(df_fixed)
  
}
