##################################.
###  Add health board region   ###
##################################.

# Author: Bex Madden
# Date: 11/07/2024

add_hb_region <- function(df){
  
  df_region <- df |> 
    mutate(hb_region = fcase(
      
      !!sym(hb_name_o) == "NHS Borders" | 
        !!sym(hb_name_o) == "NHS Fife" |
        !!sym(hb_name_o) == "NHS Lothian", "East",
      
      !!sym(hb_name_o) == "NHS Ayrshire and Arran" |
        !!sym(hb_name_o) == "NHS Greater Glasgow and Clyde" |
        !!sym(hb_name_o) == "NHS Lanarkshire" |
        !!sym(hb_name_o) == "NHS Forth Valley" |
        !!sym(hb_name_o) == "NHS Dumfries and Galloway", "West",
      
      !!sym(hb_name_o) == "NHS Western Isles" |
        !!sym(hb_name_o) == "NHS Shetland" |
        !!sym(hb_name_o) == "NHS Tayside" |
        !!sym(hb_name_o) == "NHS Highland" |
        !!sym(hb_name_o) == "NHS Grampian" |
        !!sym(hb_name_o) == "NHS Orkney", "North",
      
      default = NA_character_))
 
  return(df_region)
  
}