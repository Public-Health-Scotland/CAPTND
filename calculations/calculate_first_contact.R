

calculate_first_contact <- function(df) {
  
  
  
  df_first_contact <- df %>% 
    filter(!!sym(new_or_return_app_o)=='new - treatment start') %>% 
    select(all_of(data_keys), !!app_month_o) %>% 
    distinct() %>% 
    group_by(!!sym(app_month_o),!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    summarise(n=n(), .groups = 'drop') 
  
  w=df_first_contact %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'first_contact', save_data_board, first_contact_dir_by_board)
  
 
  write_csv_arrow(df_first_contact, paste0(first_contact_dir,'/first_contact.csv'))

  message(paste('Your files are in',first_contact_dir))
  
}
