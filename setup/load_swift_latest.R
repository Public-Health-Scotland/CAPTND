
#################################.
### Load latest SWIFT extract ###
#################################.

# Author: Charlie Smith
# Date: 2023-08-16

library(lubridate)

load_swift_latest <- function(dir_saved = "../../../output/"){
  
  df <- read_parquet(file = paste0(root_dir, "/swift_extract.parquet"))
  
  return(df)
  
}
