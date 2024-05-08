
#####################.
### Arrange DQ df ###
#####################.

# Author: Charlie Smith
# Date: 2024-05-08


arrange_dq_df <- function(df){
  
  df_arranged <- df |>  
    mutate(hb_name = factor(hb_name, levels = level_order_hb),
           variable = factor(variable, levels = vec_vars)) |>  
    arrange(header_date_month, dataset_type, hb_name, variable) |> 
    ungroup()
  
  return(df_arranged)
  
}
