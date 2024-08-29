###########################.
### Add sub_source_eval ###
###########################.


#function to find out if record spans both globalscape and swift or is restricted to just one of them
#author: JBS
#date: 01/12/23


add_sub_source_eval <- function(df){
  
  df_eval <- df %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(!!sub_source_eval_o := case_when(
      all(!!sym(sub_source_o)=='swift') ~ 'only swift',
      all(!!sym(sub_source_o)=='globalscape') ~ 'only globalscape',
      TRUE ~ 'globalscape and swift')) %>% 
    ungroup()
  
  return(df_eval)
  
}

