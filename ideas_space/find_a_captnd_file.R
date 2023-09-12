
###################################################.
### Find a specific CAPTND file based on search ### 
###################################################.

# Author: Charlie Smith
# Date: 2023-09-12


search_for_file <- function(keyword){

    folders <- list.dirs(path = ".", full.names = TRUE, recursive = FALSE) %>% # get folders in project directory 
      setdiff(., c("./.git", "./.Rproj.user")) # remove git and .Rproj folders as not required
      
    list_paths <- list()
    
    for( i in 1:length(folders)){
      files <- list.files(path = paste0(folders[i], "/"))  # get files per folder
      
      list_paths[[i]] <- data.frame(files) %>% 
        mutate(folder = paste0(folders[i]),
               filepath = paste0(folder, "/", files)) %>% 
        select(filepath)
        
      
    }
    
    df_all_paths <- bind_rows(list_paths) %>% 
      filter(str_detect(string = filepath, pattern = regex(keyword, ignore_case = FALSE))) %>% # apply keyword search
      pull() %>% 
      print()

}


# search_for_file(keyword = "ref")





