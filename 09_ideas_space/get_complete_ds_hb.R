###############################################################.
### Get df of HB names and dataset type for completing data ###
###############################################################.

# Author: Bex Madden
# date: 1/10/2024


get_complete_ds_hb <- function(inc_nhs24 = c('TRUE', 'FALSE')){

  source("./07_publication/script/functions/append_quarter_ending.R") 
    
  # create HB vector
  hb_vector <- c("NHS Ayrshire and Arran", 
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
                 "NHS 24",
                 "NHS Scotland")
  
  
  vec_dataset_type <- c("CAMHS", "PT")
  
  # create complete ds 
  df_ds_hb_name <- cross_join(as.data.frame(vec_dataset_type), 
                              as.data.frame(hb_vector)) |> 
    rename(dataset_type = vec_dataset_type,
           hb_name = hb_vector) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    filter(!(dataset_type == "CAMHS" &
             hb_name == "NHS 24")) # remove invalid combo
  
  # remove NHS 24 if desired
  if(inc_nhs24 == FALSE){
    df_ds_hb_name <- filter(df_ds_hb_name, !hb_name ==  "NHS 24")
  } else {}
  
  return(df_ds_hb_name)
}