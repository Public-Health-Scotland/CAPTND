

calculate_first_treatment <- function(df) {
  
  df_first_treatment <- df %>% 
    remove_borders_int_refs() |>
    filter(!!sym(new_or_return_app_o)=='new - treatment start') %>% 
    select(all_of(data_keys), !!app_month_o) %>% 
    distinct() %>% 
    group_by(!!sym(app_month_o),!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    summarise(n=n(), .groups = 'drop') 
  
  w=df_first_treatment %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'first_treatment', save_data_board, first_contact_dir_by_board)
  
  write_csv_arrow(df_first_treatment, paste0(first_contact_dir,'/first_treatment.csv'))
  
  message(paste('Your files are in',first_contact_dir))
  
}
