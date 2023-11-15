###############################.
##  save data board funcion  ##
###############################.

#Saves data from each board, inside a folder with the board's name, in a specific folder 

save_data_board <-function(df_board,issue,dir_name){
  
  df_name_dataset_type <- df_board %>%
    select(!!hb_name_o,!!dataset_type_o) %>%
    distinct() %>%
    unite(full_name,c(!!hb_name_o,!!dataset_type_o),sep = "_") %>%
    pull(full_name)

  write_csv_arrow(cars, paste0(dir_name,
                             '/',
                             df_name_dataset_type,
                             '_',
                             issue,
                             '.csv'))
  
  
}