
##########################.
### Save as "raw" data ###
##########################.

# Author: Charlie Smith
# Date: 2024-04-04


save_captnd_raw <- function(df){
  
  create_captnd_directory_structure()
  save_as_parquet(df, path = paste0(data_prep_dir, '/captnd_raw'))
  
}


