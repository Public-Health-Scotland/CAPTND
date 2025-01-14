
##################################.
### Filter and wrangle DQ data ###
##################################.

# Author: Charlie Smith
# Date: 2024-05-03

filter_wrangle_data <- function(df){
  
  #month_latest <- max(df$header_date_month)
  #month_range <- seq.Date(from = month_latest-months(15), to = month_latest, by = "month")
  #vec_timeframe
  
  df_long <- df |> 
    filter(header_date_month %in% vec_timeframe) |> 
    select(header_date_month, !!!syms(c(hb_name_o, dataset_type_o,  record_type_o, ucpn_o, upi_o)), starts_with("check_")) |> 
    pivot_longer(starts_with("check_"), names_to = "variable", values_to = "value") |>  
    mutate(variable = gsub("check_", "", variable)) |> 
    filter(!is.na(value)) |> # remove NAs - not sure if good idea here as then need to add back
    mutate(value = case_when(
      value %in% c("valid", "Valid") ~ "known",
      TRUE ~ value),
      #variable = factor(variable, levels = vec_var_order), # create constant
      !!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    select(!!!syms(c(dataset_type_o, hb_name_o)), everything())
  
  return(df_long)
  
}