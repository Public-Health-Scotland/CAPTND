
# Author: Luke Taylor
# Date: 2025-07-14

#Count the mean number of unattended appointments before the first attended app by health board name and 
# referral year 

 #identify first contact in pathway
  df_first_contact <- df %>%
    remove_borders_int_refs() %>%
    filter(!!sym(att_status_o) == 1 &
             !is.na(app_date_o)) %>%
    arrange(ucpn, app_date) %>%
    group_by(!!!syms(data_keys)) %>%
    slice_head() %>%
    ungroup() %>%
    select(!!!syms(data_keys), app_date) %>%
    mutate(flag = 1) %>%
    ungroup()
  
  
  #count unattended appts before first attended appointment in pathway by referral year and hb
  df_app_count <- df |>
    remove_borders_int_refs() |>
    filter(!is.na(app_date)) |>
    arrange(ucpn, app_date) |>
    select(!!!syms(data_keys), app_month, app_date, ref_rec_date_opti, app_purpose, att_status) |>
    mutate(ref_year = lubridate::year(ref_rec_date_opti)) |>
    left_join(df_first_contact, by = c('dataset_type', 'hb_name', 'ucpn', 'patient_id', 'app_date')) |>
    group_by(!!!syms(data_keys)) |>
    fill("flag", .direction = "down") |>
    filter(is.na(flag)) |>
    ungroup() |>
    group_by(ucpn, ref_year, hb_name, dataset_type) |>
    summarise(n = n(), .groups = 'drop') |>
    group_by(ref_year, hb_name, dataset_type) |>
    summarise(avg = round(mean(n),2), .groups = 'drop') |>
    arrange(ref_year, hb_name)
  
  
  
