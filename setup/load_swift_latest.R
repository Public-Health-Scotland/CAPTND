
#################################.
### Load latest SWIFT extract ###
#################################.

# Author: Charlie Smith
# Date: 2023-08-16

library(lubridate)

load_swift_latest <- function(dir_saved = "../../../output/"){
  
  DATA_FOLDER_LATEST <<- list.files(dir_saved) %>%  # list folders in directory
    grep("swift_extract", ., value = TRUE) %>% # get only extract folders
    str_replace(., "swift_extract_", "") %>% # remove "extract_" 
    max(ymd(.)) # get latest folder for filepath
  
  df <- read_parquet(file = paste0("../../../output/swift_extract_", DATA_FOLDER_LATEST, "/swift_extract.parquet"))
  
  return(df)
  
}
