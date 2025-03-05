
###########################.
### Add Scotalnd Totals ###
###########################.

# Author: Charlie Smith
# Date: 2024-05-08


add_scotland_totals <- function(df){
  
  df_sco <- df |> 
    group_by(header_date_month, dataset_type, variable, value) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop"))
  
  return(df_sco)
  
}
