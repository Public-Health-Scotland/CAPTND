#first contact is any contact with the board that patient attended
#this includes assessment apps

calculate_first_contact <- function(df) {
  
  df_first_contact <- df %>%
    remove_borders_int_refs() %>%
    filter(!!sym(att_status_o) == 1 &
             !is.na(app_date_o)) %>%
    arrange(ucpn, app_date) %>%
    group_by(!!!syms(data_keys)) %>%
    slice_head() %>%
    ungroup() %>%
    group_by(app_month, hb_name, dataset_type) %>%
    summarise(n = n(), .groups = 'drop') %>% 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(app_month %in% month_range) %>%
    group_by(app_month, !!sym(dataset_type_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))
  
  write_csv_arrow(df_first_contact, paste0(first_contact_dir,'/first_contact.csv'))

  message(paste('Your files are in',first_contact_dir))
  
}
