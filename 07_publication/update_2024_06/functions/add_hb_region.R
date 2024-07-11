##################################.
###  Add health board region   ###
##################################.

# Author: Bex Madden
# Date: 11/07/2024

add_hb_region <- function(df){
  
  df_with_region <- df |> 
    mutate(hb_region = case_when(
      hb_name == "NHS Borders" | 
        hb_name == "NHS Fife" |
        hb_name == "NHS Lothian" ~ "East region",
      
      hb_name == "NHS Ayrshire and Arran" |
        hb_name == "NHS Greater Glasgow and Clyde" |
        hb_name == "NHS Lanarkshire" |
        hb_name == "NHS Forth Valley" |
        hb_name == "NHS Dumfries and Galloway" ~ "West region",
      
      hb_name == "NHS Western Isles" |
        hb_name == "NHS Shetland" |
        hb_name == "NHS Tayside" |
        hb_name == "NHS Highland" |
        hb_name == "NHS Grampian" |
        hb_name == "NHS Orkney" ~ "North region",
      
      TRUE ~ NA_character_
    ))
}