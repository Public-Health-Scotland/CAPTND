########################################################
## Calculate First Contact DNAs for Comparison Report ##
########################################################


calculate_first_contact_dnas <- function(df){
  
  first_contact <- df |>
    arrange(!!ucpn_o, !!app_date_o) |>
    filter(!!sym(att_status_o) == 1) |>
    group_by(!!!syms(data_keys)) |> 
    select(all_of(data_keys), !!sym(app_date_o)) |>
    slice(1) |>
    mutate(first_att_contact = !!sym(app_date_o)) |>
    select(-!!sym(app_date_o))
  
  
  df_first_contact_dnas <- df |>
    arrange(!!ucpn_o, !!app_date_o) |>
    filter(!!sym(att_status_o) == 8) |>
    select(all_of(data_keys), !!sym(app_date_o)) |>
    left_join(first_contact, by = c('dataset_type', 'hb_name', 'patient_id', 'ucpn')) |> #First attended contact
    mutate(diff_time = as.numeric(first_att_contact - app_date),
           contact_month = floor_date(!!sym(app_date_o), unit = 'month')) |>
    filter(diff_time >= 0 | is.na(diff_time)) |> # Filter for all DNAs before first attended contact
    group_by(contact_month, !!sym(hb_name_o), !!sym(dataset_type_o)) |>
    summarise(n = n(), .groups = 'drop') |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    group_by(contact_month, !!sym(dataset_type_o)) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop")) |> 
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o))
  
  write_csv_arrow(df_first_contact_dnas, paste0(dna_dir, '/first_contact_dnas.csv'))
  
  message(paste('Your files are in', dna_dir))
    
}


