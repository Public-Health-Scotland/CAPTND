#####################################.
###  Details of unusable records  ###
#####################################.

# 1 Load libraries and packages ---------------------------------------------
library(dplyr)
library(lubridate)
library(arrow)
source('config/new_colnames.R')


# 2 Function ----------------------------------------------------------------

report_unusable_records_removed <- function(){
  
  save_removed_data_board <-function(df){
    
    df_name <- df %>% select(!!hb_name_o) %>% distinct() %>% pull(!!hb_name_o)
    write_csv_arrow(df, paste('../../../output/removed/details_removed/by_board/',
                              df_name,
                              '_data_removed_details.csv'))
    
  }
  
  
  df <- list.files(path = "../../../output/removed/details_removed/", pattern ="swift.*\\.csv$", full.names = TRUE) %>% 
    map(~.[str_detect(.,as.character(DATA_FOLDER_LATEST))]) %>% 
    map_df(~read_csv(.,show_col_types = FALSE)) %>% 
    group_by(!!sym(hb_name_o)) %>% 
    group_split() %>% 
    map(save_removed_data_board)
  
}