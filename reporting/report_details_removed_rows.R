#####################################.
###  Details of unusable records  ###
#####################################.

# 1 Load libraries and packages ---------------------------------------------
# library(dplyr)
# library(lubridate)
# library(arrow)
source('config/new_colnames.R')


# 2 Function ----------------------------------------------------------------

report_removed_rows_details <- function(){
  
  save_removed_data_board <-function(df){
    
    df_name <- df %>% select(!!hb_name_o) %>% distinct() %>% pull(!!hb_name_o)
    
    df <- df %>% mutate(!!header_date_o := as.character(format(!!sym(header_date_o), "%Y-%m-%d")))
    write_csv_arrow(df, paste0(removed_data_export_by_board_dir,'/',
                              df_name,
                              '_data_removed_details.csv'))
    
  }
  
  
  df <- list.files(path = paste0(removed_data_export_dir,'/'), pattern ="swift.*\\.csv$", full.names = TRUE) %>% 
    #map(~.[str_detect(.,as.character(DATA_FOLDER_LATEST))]) %>% 
    map_df(~read_csv(.,show_col_types = FALSE)) %>% 
    group_by(!!sym(hb_name_o)) %>% 
    group_split() %>% 
    map(save_removed_data_board)
  
}