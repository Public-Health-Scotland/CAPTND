
###################################################.
### Find a specific CAPTND file based on search ### 
###################################################.

# Author: Charlie Smith
# Date: 2023-09-12


search_for_file <- function(keyword){

    folders <- list.files(path = ".", full.names = TRUE, recursive = TRUE, pattern = ".R$") # get folders in project directory 

    df_all_paths <- data.frame(folders) %>% 
      filter(str_detect(string = folders, pattern = regex(keyword, ignore_case = FALSE))) %>% # apply keyword search
      pull() %>% 
      print()

}

# search_for_file(keyword = "ref")





