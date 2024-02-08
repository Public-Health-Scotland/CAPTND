##########################.
### Find analysis date ###.
##########################.

#Joana Bittencourt Silvestre
#14/11/23

data_analysis_latest_date <- list.dirs('../../../output', recursive = FALSE, full.names = FALSE) %>%  # list folders in directory
    grep("analysis", ., value = TRUE) %>% # get only extract folders
    str_replace(., "analysis_", "") %>% # remove "extract_" 
    max(ymd(.)) # get latest folder for filepath
  