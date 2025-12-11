
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

updated_hb_vector <- c("NHS Ayrshire and Arran", 
               "NHS Borders", 
               "NHS Dumfries and Galloway",
               "NHS Fife",
               "NHS Forth Valley", 
               "NHS Grampian",
               "NHS Greater Glasgow and Clyde",
               "NHS Highland",
               "NHS Lanarkshire", 
               "NHS Lothian", 
               "NHS Orkney",
               "NHS Shetland", 
               "NHS Tayside", 
               "NHS Western Isles",
               "NHS24",
               "NHSScotland")
