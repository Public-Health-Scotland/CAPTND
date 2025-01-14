
#######################################.
### Complete df with missing values ###
#######################################.

# Author: Charlie Smith
# Date: 2024-05-07

# Items to cover:
# Months
# Datasets
# HBs
# Variables


complete_absent_vars_na <- function(df){
  
  # make relevant vectors dfs for easier joining
  df_timeframe <- data.frame(header_date_month = vec_timeframe) 
  df_dataset_type <- data.frame(dataset_type = vec_dataset_types)
  df_hb_base <- data.frame(hb_name = level_order_hb)
  df_variable <- data.frame(variable = vec_vars)
  df_value <- data.frame(value = vec_value)
  
  # cross_join to get all possible values
  df_full_vars <- df_timeframe |> 
    cross_join(df_dataset_type) |>  
    cross_join(df_hb_base) |>  
    cross_join(df_variable) |> 
    cross_join(df_value) |>  
    select(header_date_month, dataset_type, hb_name, variable, value)
  
  # get df of values absent from captnd subs
  df_absent <- anti_join(df_full_vars, df) %>% 
    mutate(count = 0)
  
  # stack df_present and df_absent so that all combis are complete
  df_stack_all <- rbind(df, df_absent) |>
    group_by(header_date_month, dataset_type, hb_name, variable) |> 
    arrange(header_date_month, dataset_type, hb_name, variable, value) |> 
    ungroup()
  
  return(df_stack_all)
  
}
