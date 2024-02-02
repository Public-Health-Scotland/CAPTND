#first contact is any contact with the board that patient attended
#this includes assessment apps

calculate_first_contact <- function(df) {
  
  
  
  df_first_contact <- df %>% 
    filter(str_detect(!!sym(new_or_return_app_o), 'new') &
             !!sym(att_status_o) == 1) %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(first_contact_month=min(!!sym(app_month_o))) %>% 
    select(all_of(data_keys), first_contact_month) %>% 
    distinct() %>% 
    group_by(first_contact_month,!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    summarise(n=n(), .groups = 'drop') 
  
  w=df_first_contact %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'first_contact', save_data_board, first_contact_dir_by_board)
  
 
  write_csv_arrow(df_first_contact, paste0(first_contact_dir,'/first_contact.csv'))

  message(paste('Your files are in',first_contact_dir))
  
}
