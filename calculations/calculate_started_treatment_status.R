
### this has been paused for now ###


library(dplyr)
library(lubridate)
library(arrow)
library(phsmethods)
library(conflicted)
source('calculations/save_data_board.R')
conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')
conflict_prefer('summarise', 'dplyr')


calculate_patients_seen <- function(df_glob_swift_completed_rtt) {
  
  df_treat_status <- df_glob_swift_completed_rtt |> 
    filter(str_detect(!!sym(rtt_eval_o), 'seen')) |> 
    select(all_of(data_keys),!!ref_rec_date_opti_o, !!rtt_eval_o, sub_source_eval) %>% 
    distinct()
  
  y=df_treat_status %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'started_treatment_detail', save_data_board, started_treatment_dir_by_board)
  
  message(paste0('Your output files are in ', started_treatment_dir))
  
}




