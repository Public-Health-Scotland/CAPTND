
################################################.
### Identify disparities in package versions ###
################################################.

# Author: Charlie Smith
# Date: 2024-11-12


# Proof of concept
# df1 <- data.frame(
#   user_name = c("charlie", "charlie", "charlie"),
#   package = c("dplyr", "ggplot2", "arrow"),
#   version = c("1.2.1", "2.4.1", "4.2.4"))
# 
# df2 <- data.frame(
#   user_name = c("bex", "bex", "bex"),
#   package = c("dplyr", "ggplot2", "arrow"),
#   version = c("1.2.1", "2.4.8", "4.2.4"))
# 
# df3 <- data.frame(
#   user_name = c("luke", "luke", "luke"),
#   package = c("dplyr", "ggplot2", "arrow"),
#   version = c("1.2.1", "2.4.7", "4.2.4")) 
# 
# test <- rbind(df1, df2, df3) |> 
#   group_by(package) |> 
#   arrange(package) |> 
#   mutate(version_lagged = lag(version, n = 1), 
#          is_match = version == version_lagged) |> 
#   filter(is_match == FALSE)



compare_package_versions <- function(user_name){
  
  # user_name <- tolower(readline(prompt = 'Please enter your name: '))
  
  if( missing(user_name)){
    user_name <- tolower(readline(prompt = 'Please enter your name: '))
  }
  user_name <- tolower(user_name)
  
  suppressMessages(source("./07_publication/script/chapters/1_load_packages.R")) # load packages used in control_mmi.R
  source('./02_setup/save_df_as_parquet.R')
  
  df_sesh_info <- sessionInfo() #create session info object to pull info from
  
  # "other" packages
  list_bucket_names <- list()
  list_bucket_versions <- list()
  
  for(i in 1:length(df_sesh_info[[7]])){
        
    package_name <- df_sesh_info[[7]][[i]]$Package
    package_version <- df_sesh_info[[7]][[i]]$Version
    
    list_bucket_names[[i]] <- package_name
    list_bucket_versions[[i]] <- package_version
    
  }
  
  df_pac_versions_other <- data.frame(
    package_name = unlist(list_bucket_names),
    package_version = unlist(list_bucket_versions)
    )
  
  
  # "loaded" packages
  list_bucket_names <- list()
  list_bucket_versions <- list()
  
  for(i in 1:length(df_sesh_info[[8]])){
    
    package_name <- df_sesh_info[[8]][[i]]$Package
    package_version <- df_sesh_info[[8]][[i]]$Version
    
    list_bucket_names[[i]] <- package_name
    list_bucket_versions[[i]] <- package_version
    
  }
  
  df_pac_versions_known <- data.frame(
    package_name = unlist(list_bucket_names),
    package_version = unlist(list_bucket_versions)
  )
  
  
  # stack both other and known dfs, save with user_name in file name
  df_pac_versions <- rbind(df_pac_versions_other, df_pac_versions_known) |> 
    mutate(user = paste0(user_name), .before = everything()) |> 
    save_as_parquet(path = paste0('../../../data/package_info_', user_name))
  
  message("Your session info has been saved to '../../../data/package_info_NAME.parquet.")
  
  
  if(file.exists('../../../data/package_info_charlie.parquet') &
     file.exists('../../../data/package_info_bex.parquet') &
     file.exists('../../../data/package_info_luke.parquet')){
    
    df_comp <- rbind(
      read_parquet('../../../data/package_info_charlie.parquet'),
      read_parquet('../../../data/package_info_bex.parquet'),
      read_parquet('../../../data/package_info_luke.parquet')) |> 
      group_by(package) |>
      arrange(package) |>
      mutate(version_lagged = lag(version, n = 1),
             is_match = version == version_lagged) |>
      filter(is_match == FALSE) |> 
      View()
    
    message("This data frame shows non-matching packages only")
    
  } else {
    
    message("Couldn't make comparison. Make sure Charlie, Bex, and Luke have each run this function and that the names are spelled correctly!")
  
    }
  
}

# compare_package_versions(user_name = "charlie")

