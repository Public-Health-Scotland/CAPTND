

calculate_first_treatment <- function(df) {
  
  df_first_treatment <- df |>
    remove_borders_int_refs() |>
    filter(!!sym(att_status_o) == 1,
           !!sym(app_purpose_o) %in% c(2,3,5)) |>
    group_by(!!!syms(data_keys)) |>
    arrange(ucpn, app_date) |>
    slice(1) |>
    mutate(first_contact_month = min(!!sym(app_month_o))) |>
    select(all_of(data_keys), first_contact_month) |>
    group_by(first_contact_month,!!sym(hb_name_o),!!sym(dataset_type_o)) |>
    summarise(n = n(), .groups = 'drop') |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    group_by(first_contact_month, !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o))
  
  # df_first_treatment <- df %>% 
  #   remove_borders_int_refs() |>
  #   filter(!!sym(new_or_return_app_o)=='new - treatment start') %>% 
  #   select(all_of(data_keys), !!app_month_o) %>% 
  #   distinct() %>% 
  #   group_by(!!sym(app_month_o),!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
  #   summarise(n=n(), .groups = 'drop') 
  # 
  # w=df_first_treatment %>% 
  #   group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
  #   group_split() %>% 
  #   map2(., 'first_treatment', save_data_board, first_contact_dir_by_board)
  
  write_csv_arrow(df_first_treatment, paste0(first_contact_dir,'/first_treatment.csv'))
  
  message(paste('Your files are in',first_contact_dir))
  
}
