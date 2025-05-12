########################################################
## Calculate First Contact DNAs for Comparison Report ##
########################################################

#By: Luke Taylor
#Date: 07/02/2025

calculate_first_contact_dnas <- function(df){
  
  #identify all pathways that start with a DNA appt
  df_first_contact_dna_pathways <- df |>
    remove_borders_int_refs() |>
    filter(!is.na(app_date)) |>
    arrange(ucpn, app_date) |>
    group_by(!!!syms(data_keys)) |>
    mutate(dna_first_appt = case_when(row_number() == 1 & att_status == 8 ~ 'DNA',
                                      TRUE ~ NA)) |>
    fill("dna_first_appt", .direction = "down") |>
    filter(dna_first_appt == 'DNA') |>
    ungroup()
  
  #keep all DNA appts in pathway until first attended appt
  df_first_contact_dnas <- df_first_contact_dna_pathways |>
    group_by(!!!syms(data_keys)) |>
    mutate(first_att = case_when(att_status == 1 ~ 'Attended',
                                 TRUE ~ NA)) |>
    fill("first_att", .direction = 'down') |>
    filter(is.na(first_att),
           att_status == 8) |>
    select(!!!syms(data_keys), app_month) %>%
    group_by(app_month, hb_name, dataset_type) %>%
    summarise(n = n(), .groups = 'drop') %>% 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    filter(app_month %in% month_range) %>%
    group_by(app_month, !!sym(dataset_type_o)) %>%
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!sym(hb_name_o), ~"NHS Scotland"),
                        .groups = "drop"))

  write_csv_arrow(df_first_contact_dnas, paste0(dna_dir, '/first_contact_dnas.csv'))

  message(paste('Your files are in', dna_dir))
    
}


