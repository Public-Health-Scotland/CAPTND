
##################################.
### Change 'NHSScotland' label ###
##################################.

# Author: Luke Taylor
# Date: 2024-11-11

change_nhsscotland_label <- function(df){
  
  df_hb_update <- df |>
    mutate(hb_name = case_when(hb_name == 'NHS Scotland' ~ 'NHSScotland',
                               TRUE ~ hb_name))
  
  return(df_hb_update)
  
}