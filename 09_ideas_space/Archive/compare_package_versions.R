
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



compare_package_versions <- function(user_name, to_compare = c("user1", "user2")){
  
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
  #df_pac_versions <- df_pac_versions_other |> 
    mutate(user = paste0(user_name), .before = everything()) |> 
    save_as_parquet(path = paste0('../../../data/package_info_', user_name))
  
  message(paste0("Your session info has been saved to '../../../data/package_info_", user_name,".parquet."))
  
  
  if(file.exists('../../../data/package_info_charlie.parquet') &
     file.exists('../../../data/package_info_bex.parquet') &
     file.exists('../../../data/package_info_luke.parquet')){
    
    # combine user package info dfs
    df_joined <- rbind(
      #read_parquet('../../../data/package_info_bex.parquet'),
      read_parquet(paste0('../../../data/package_info_', tolower(to_compare[1]), '.parquet')),
      read_parquet(paste0('../../../data/package_info_', tolower(to_compare[2]), '.parquet'))) |> 
      ungroup()
    
    # identify missing packages from one user(will cause problems)  
    #df_users <- df_joined |> select(user) |> pull() |> unique()
      
    vec_user1_packs <- df_joined |> 
      filter(user == to_compare[1]) |> 
      select(package_name) |> pull()
    
    vec_user2_packs <- df_joined |> 
      filter(user == to_compare[2]) |> 
      select(package_name) |> pull()
    
    vec_pacs_missing <- append(setdiff(vec_user1_packs, vec_user2_packs), 
                               setdiff(vec_user2_packs, vec_user1_packs))
    
    # user1_has_user2_doesnt <- data.frame(
    #   user = df_users[2],
    #   package_name = setdiff(vec_user1_packs, vec_user2_packs),
    #   package_version = "not_installed"
    #   )
    # 
    # user2_has_user1_doesnt <- data.frame(
    #   user = df_users[1],
    #   package_name = setdiff(vec_user2_packs, vec_user1_packs),
    #   package_version = "not_installed"
    # )

    #df_comp <- rbind(df_joined, user1_has_user2_doesnt, user2_has_user1_doesnt) |> 
    df_comp <- df_joined |> 
      group_by(package_name) |>
      arrange(package_name) |>
      mutate(comp_version = lag(package_version, n = 1),
             assessment = case_when(
               package_version == comp_version ~ "match", 
               package_version != comp_version ~ "mismatch",
               is.na(comp_version) & package_name %in% vec_pacs_missing ~ "not_installed/loaded",
               TRUE ~ NA_character_)) |>
      filter(assessment %in% c("mismatch", "not_installed/loaded")) |> 
      ungroup() |> 
      arrange(user, package_name)
    
    View(df_comp)
    assign(x = "df_comp", value = df_comp, envir = .GlobalEnv)
    
    message("This data frame (df_comp) shows non-matching or not installed packages only.")
    
  } else {
    
    message("Couldn't make comparison. Make sure Charlie, Bex, and Luke have each run this function and that the names are spelled correctly!")
  
    }
  
}

# Example - get session info for charlie, then compare package versions of charlie and luke:
# compare_package_versions(user_name = "charlie",
#                          to_compare = c("charlie", "luke"))

