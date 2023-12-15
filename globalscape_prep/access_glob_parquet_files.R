
#############################################.
### Load CAPTND Globalscape Parquet Files ###
#############################################.

# Purpose: Load saved parquet files
# Date: 2023-06-01
# Author: Charlie Smith

# library(lubridate)
# library(arrow)
# library(dplyr)

# 1 - Load data -----------------------------------------------------------

load_glob_parquet_dfs <- function() {
  
  out_dir <- "../../../output/" 
  
  # get latest data folder - MAKE FUNCTION
  folders <- list.files(out_dir, pattern = "glob_extract") 
  folders <- gsub("glob_extract_", "", folders)
  
  dated_folder <- max(ymd(folders))
  rm(folders)
  
  dated_folder <- paste0(out_dir, "glob_extract_", as.character(dated_folder),"/")
  
  # load data
  parq_list <- list.files(dated_folder)
  
  parq_dfs <- vector(mode = "list", length = length(parq_list))
  
  for(i in seq_along(parq_list)){
    x <- read_parquet(paste0(dated_folder, parq_list[i]))
    parq_dfs[[i]] <- x
  }
  
  names(parq_dfs)= gsub('.parquet','',parq_list)
  
  return(parq_dfs)
  
  # for(i in seq_along(parq_list)){
  #   
  #   x <- read_parquet(paste0(dated_folder, parq_list[i]))
  #   
  #   obj_name <- gsub(pattern = ".parquet", "", parq_list[i]) 
  #   assign(obj_name, as.data.frame(x), envir = as.environment(globalenv()))
  # 
  #   
  # }

}


