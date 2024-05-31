
###################################.
### Summarise by quarter ending ###
###################################.

# Author: Charlie Smith 
# Date: 2024-05-30

# group_vec = c("quarter_ending", "dataset_type", "ref_source_desc")
# df = df_month_hb

summarise_by_quarter <- function(df, vec_group){
  
  df_quarter <- df |> 
    group_by(across(all_of(vec_group))) |> 
    summarise(count = sum(count))
    
  return(df_quarter)
  
}


