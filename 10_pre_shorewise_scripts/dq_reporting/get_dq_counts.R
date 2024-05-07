
#####################.
### Get DQ counts ###
#####################.

# Author: Charlie Smith
# Date: 2024-05-03

get_dq_counts <- function(df){
  
  df_calcs <- df |> 
    filter(value != "missing but valid") |>  # remove validly missing values from REJ variables
    group_by(header_date_month, !!!syms(c(dataset_type_o, hb_name_o)), variable, value) |>  
    summarise(count = n()) |>  
    arrange(header_date_month, dataset_type, hb_name, variable) 
  
  return(df_calcs)
  
}