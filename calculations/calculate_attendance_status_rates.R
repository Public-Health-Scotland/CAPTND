
#####################################################.
### Calculate Appointment Attendance Status Rates ###
#####################################################.

# Author: Charlie Smith
# Date: 2023-11-22

# DNA, CNA, and Rescheduled rates by Dataset, HB, appointment month and attendance category 

calculate_attendance_status_rates <- function(df){
  
  df_rates <- df |> 
    filter(!is.na(!!sym(app_date_o))) |>  # must have app_date
    mutate(
           # add attendance category description
           !!sym(att_cat_desc_o) := case_when(
             !!sym(att_cat_o) == 1 ~ "new appointment",
             !!sym(att_cat_o) == 2 ~ "return appointment",
             TRUE ~ "no info"),
           
           # add attendance status description       
           !!sym(att_status_desc_o) := case_when(
             !!sym(att_status_o) == 1 ~ "seen",
             !!sym(att_status_o) == 2 ~ "cancelled by NHS",
             !!sym(att_status_o) == 3 ~ "CNA",
             !!sym(att_status_o) == 8 ~ "DNA",
             is.na(!!sym(att_status_o)) ~ "no info",
             TRUE ~ "other")) |> 
    
    # calculate rates
    group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o, att_cat_desc_o, att_status_desc_o))) |> 
    summarise(app_count = n()) |> 
    ungroup() |> 
    group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o,  att_cat_desc_o))) |> 
    mutate(app_total = sum(app_count, na.rm = TRUE),
           att_status_rate = round( app_count / app_total * 100, 1))
  
  # save output as csv
  write_csv_arrow(df_rates, paste0(dna_dir, '/attendance_status_rates.csv'))
  
  message(paste0('Your output files are in ', dna_dir))
  
  # save output by Health Board
  x=df_rates %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'attendance_status_rates', save_data_board, dna_dir_by_board)
  
}

