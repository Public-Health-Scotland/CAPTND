
#################################.
### Load latest SWIFT extract ###
#################################.

# Author: Charlie Smith
# Date: 2023-08-16

library(lubridate)

load_swift_latest <- function(dir_saved = "../../../output/"){
  
  data_folder_latest <- list.files(dir_saved) %>%  # list folders in directory
    grep("extract", ., value = TRUE) %>% # get only extract folders
    str_replace(., "extract_", "") %>% # remove "extract_" 
    max(ymd(.)) # get latest folder for filepath
  
  df <- read_parquet(file = paste0("../../../output/extract_", data_folder_latest, "/swift.parquet"))
  
  return(df)
  
}
