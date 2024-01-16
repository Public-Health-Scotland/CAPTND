

#####################################################.
### Calculate Appointment Attendance Status Rates ###
#####################################################.

# Author: Charlie Smith
# Date: 2023-11-22

# DNA, CNA, and Rescheduled rates by Dataset, HB, appointment month and attendance category 
# TESTING: broken down by Sex, Age, and SIMD quintile

calculate_attendance_status_rates <- function(df){
  
  # add descriptive labels to df
  df_att_status <- df |> 
    filter(!is.na(!!sym(app_date_o))) |>  # must have app_date
    # add attendance status description      
    mutate(!!sym(att_status_desc_o) := case_when(
      !!sym(att_status_o) == 1 ~ "seen",
      !!sym(att_status_o) == 2 ~ "cancelled by NHS",
      !!sym(att_status_o) == 3 ~ "CNA",
      !!sym(att_status_o) == 8 ~ "DNA",
      is.na(!!sym(att_status_o)) ~ "no info",
      TRUE ~ "other"))  
  
  
  # calculate rates
  
  # 1 - count ---------------------------------------------------------------
  df_count <- df_att_status |> 
    # count by hb
    group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o, new_or_return_app_o, att_status_desc_o))) |> 
    summarise(app_count = n()) |> 
    ungroup() |> 
    # count for all HBs combined (NHS Scotland)
    group_by(!!!syms(c(dataset_type_o, app_month_o, new_or_return_app_o, att_status_desc_o))) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(!!hb_name_o, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    # add totals and calculate percentages
    group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o, new_or_return_app_o))) |> 
    mutate(app_total = sum(app_count, na.rm = TRUE),
           att_status_rate = round( app_count / app_total * 100, 1),
           count_by_desc = "none", # column added for later row binding
           count_by_code = NA_character_) # column added for later row binding
  
  
  
  ### REFACTOR WITH LOOP
  vec_demos <- c(sex_reported_o, age_group_o, simd_quintile_o)
  
  list_bucket <- list() # create empty list to store changes
  
  for(i in 1:length(vec_demos)){
    
    df_count_demos <- df_att_status |> 
      # count by HB
      group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o, vec_demos[i], new_or_return_app_o, 
                         att_status_desc_o))) |> 
      summarise(app_count = n()) |>
      ungroup() |>
      # add count by NHS Scotland
      group_by(!!!syms(c(dataset_type_o, app_month_o, vec_demos[i], new_or_return_app_o, att_status_desc_o))) %>%
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(!!hb_name_o, ~"NHS Scotland"),
                          .groups = "drop")) |>
      # add totals and percentages
      group_by(!!!syms(c(dataset_type_o, hb_name_o, app_month_o, vec_demos[i], new_or_return_app_o))) |>
      mutate(app_total = sum(app_count, na.rm = TRUE),
             att_status_rate = round( app_count / app_total * 100, 1)) |>
      pivot_longer(cols = sym(vec_demos[i]),
                   names_to = "count_by_desc", # pivot longer to enable all measure rowbind
                   values_to = "count_by_code") |>
      mutate(count_by_code = as.character(count_by_code))
    
    list_bucket[[i]] <- df_count_demos # store loop df in list
    
  }
  
  # combine all df to one for saving
  df_rates2 <- bind_rows(df_count, list_bucket) |>  
    ungroup() |> 
    select(1:3, 9, 10, 4:8) 
  
  # save output as csv
  write_csv_arrow(df_rates, paste0(dna_dir, '/attendance_status_rates.csv'))
  
  message(paste0('Your output files are in ', dna_dir))
  
  # save output by Health Board
  x=df_rates %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    group_split() %>% 
    map2(., 'attendance_status_rates', save_data_board, dna_dir_by_board)
  
}

