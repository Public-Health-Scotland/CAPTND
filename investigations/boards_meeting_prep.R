library(dplyr)
library(lubridate)
library(arrow)
library(conflicted)
source('config/new_colnames.R')
source('setup/save_df_as_parquet.R')


prep_board_meeting_brief <- function(df_eval){
  
  df_grouped =df_eval %>% 
    select(all_of(data_keys),rtt_eval,sub_source_eval) %>% 
    distinct() %>% 
    filter(!rtt_eval %in% c('complete rtt',#1
                            'patient waiting',#2
                            'ref rej',#3
                            'online treatment')) %>% 
    group_by(!!sym(hb_name_o)) %>% 
    group_split()
  
  
  save_grouped_df <- function(df_from_group){
    
    save_name <- df_from_group %>% select(!!hb_name_o) %>% distinct() %>% pull(!!hb_name_o)
    
    write_csv_arrow(df_from_group, paste0('../../../output/investigations/RTT_evaluation_board/rrt_eval_',
                                           save_name,
                                           '_details.csv'))
  }
  
  
  x=map(df_grouped, save_grouped_df)
  
}

