
##########################################.
### Append NHSScotland label as factor ###
##########################################.

# Author: CJS
# Date: 2024-12-10

# Note: this new name is so daft

append_nhsscotland_label_factor <- function(df){
  
  df <- df |> 
    mutate(hb_name = if_else(hb_name == "NHS Scotland", "NHSScotland", hb_name),
           hb_name = factor(hb_name, levels = level_order_hb3))
  
  return(df)
  
}

