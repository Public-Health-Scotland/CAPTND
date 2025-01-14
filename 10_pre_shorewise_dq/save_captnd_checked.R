
##############################.
### Save as "checked" data ###
##############################.

# Author: Charlie Smith
# Date: 2024-04-12


save_captnd_checked <- function(df){
  
  #create_captnd_directory_structure()
  save_as_parquet(df, path = paste0(data_prep_dir, '/captnd_checked'))
  
}

