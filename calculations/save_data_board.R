###############################.
##  save data board funcion  ##
###############################.

#Saves data from each board, inside a folder with the board's name, in a specific folder 

save_data_board <-function(df,issue,folderName){
  
  df_name_dataset_type <- df %>% 
    select(!!hb_name_o,!!dataset_type_o) %>% 
    distinct() %>% 
    unite(full_name,c(!!hb_name_o,!!dataset_type_o),sep = "_") %>% 
    pull(full_name) 
  
  write_csv_arrow(df, paste0('../../../output/calculations/',
                             folderName,
                             '/byBoard/',
                             df_name_dataset_type,
                             '_',
                             issue,
                             '.csv'))
  
}