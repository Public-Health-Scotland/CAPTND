
#####################################.
### open last rtt evaluated file  ###
#####################################.

#This takes the last rtt evaluated file and calculates different measure from it


library(dplyr)
library(purrr)
library(stringr)
library(lubridate)


open_last_parquet_with_rrt_eval <- function() {
  
  
  last_date_on_file <- list.files(path = "../../../output/", pattern ="df_glob_swift_completed_rtt_.*\\.parquet$", full.names = FALSE) %>% 
    map_chr(~str_extract(.,'\\d{4}.+\\d{2}.+\\d{2}')) %>% max(.)
  
  filename=paste0("../../../output/df_glob_swift_completed_rtt_",as.character(last_date_on_file) ,".parquet")
  
  df=read_parquet(filename)
  
  return(df)

}

