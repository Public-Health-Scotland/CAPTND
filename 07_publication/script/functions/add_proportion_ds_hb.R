
########################################################.
### Add proportion based on dataset type and hb name ###
########################################################.

# Author: Charlie Smith 
# Date: 2024-06-03


add_proportion_ds_hb <- function(df, vec_group = c("dataset_type", "hb_name")){
  
  df_prop <- df |> 
    group_by(across(all_of(vec_group))) |> 
    mutate(total = sum(count),
           prop = round ( count / total * 100 , 1))
  
  return(df_prop)
  
}


