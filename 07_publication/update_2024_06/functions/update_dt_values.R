
################################.
### Update data table values ###
################################.

# Author: Charlie Smith
# Date: 2024-08-13

update_dt_values <- function(wb){
  
  # get quarters ending for all dts
  df_quarts <- read_parquet(paste0(ref_dir, "referrals_quarter_hb.parquet")) |> 
    ungroup() |> select(quarter_ending) |> unique() #|> pull()
  
  df_qt_ds_hb <- df_ds_hb_name |> cross_join(df_quarts)
  quarter_range <- df_quarts |> pull()
  
  # based on dataset_choice...
  
  
  
  # replace CAMHS in lookup with PT
  if(dataset_choice == "PT"){
    writeData(wb, sheet = "Lookups", 
              x = "PT",  
              startCol = 2, startRow = 2, #headerStyle = style_text, 
              colNames = FALSE)
  }
  
  # quarterly referrals by HB
  # df_refs <- read_parquet(paste0(ref_dir, "referrals_quarter_hb.parquet")) |> 
  #   right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |>
  #   mutate(hb_name = factor(hb_name, levels = hb_vector)) |>
  #   arrange(dataset_type, hb_name) |>
  #   rename(`Health board` = hb_name) |>
  #   filter(dataset_type == dataset_choice) 
  
  df_refs <- read_parquet(paste0(ref_dir, "referrals_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    arrange(dataset_type, hb_name) |> 
    right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |> 
    rename(`Health board` = hb_name) |> 
    filter(dataset_type == dataset_choice)
  
  writeData(wb, sheet = "Tab 1 Data", 
            x = df_refs,  
            startCol = 2, startRow = 2, #headerStyle = style_text, 
            colNames = FALSE)
  addStyle(wb, sheet = "Tab 1", style = style_count, cols = 3, rows = 14:18, stack = TRUE)
  
  writeData(wb, sheet = "Tab 1", 
            x = df_quarts,  
            startCol = 2, startRow = 14:18, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 1", style = style_date, cols = 2, rows = 14:18, stack = TRUE)

  # quarterly referral acceptance status by HB
  # df_acc_status <- read_parquet(paste0(non_acc_dir, "non_acceptance_summary_quarter_hb.parquet")) |> 
  #   filter(dataset_type == dataset_choice) 


  df_acc_status <- read_parquet(paste0(non_acc_dir, "non_acceptance_summary_", "quarter_hb.parquet")) |> 
    ungroup() |> 
    filter(quarter_ending %in% quarter_range) |> 
    select(-total, -prop) |> 
    pivot_wider(names_from = ref_acc_desc, values_from = count,
                values_fill = 0) |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    pivot_longer(cols = 4:7, values_to = "count", names_to = "ref_acc_desc") |> 
    group_by(quarter_ending, dataset_type, hb_name) |>
    mutate(total = sum(count, na.rm = TRUE),
           prop = round(count / total * 100, 1)) |>
    
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    filter(dataset_type == dataset_choice) 
  
  
  writeData(wb, sheet = "Tab 2 Data", 
            x = df_acc_status,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_count, cols = 3, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 2", style = style_count, cols = 4, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 2", style = createStyle(halign = "right"), cols = 5, rows = 15:19, stack = TRUE)
  
  writeData(wb, sheet = "Tab 2", 
            x = df_quarts,  
            startCol = 2, startRow = 15:19, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 2", style = style_date, cols = 2, rows = 15:19, stack = TRUE)
  
  
  # quarterly appointment attendance by HB 
  # first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |> 
  #   # rename(quarter_ending = app_quarter_ending) |> 
  #   # right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |>
  #   # mutate(hb_name = factor(hb_name, levels = hb_vector)) |>
  #   # arrange(dataset_type, hb_name) |>
  #   # rename(`Health board` = hb_name) |>
  #   filter(dataset_type == dataset_choice) 
  
  first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |> 
    select(-prop_firstcon_att) |> 
    pivot_wider(names_from = Attendance, values_from = firstcon_att, values_fill = 0) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> 
    arrange(dataset_type, hb_name) |> 
    pivot_longer(cols = 6:12, names_to = "att_status", values_to = "count") |> 
    mutate(prop = round(count / first_contact * 100, 1)) |> 
    select(dataset_type, hb_name, app_quarter_ending, att_status, count, first_contact, prop, total_apps) |> 
    # mutate(att_status = factor(att_status, 
    #                            levels = c("Attended", "Patient DNA", "Patient cancelled",
    #                                       "Clinic cancelled", "Patient CNW", "Not known"))) |> 
    # arrange(dataset_type, hb_name, app_quarter_ending, att_status)
    filter(dataset_type == dataset_choice) 
  
  
  writeData(wb, sheet = "Tab 3 Data", 
            x = first_att_latest,  
            startCol = 2, startRow = 2, headerStyle = style_text, colNames = FALSE)
  addStyle(wb, sheet = "Tab 3", style = style_count, cols = 3, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 3", style = style_count, cols = 4, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 3", style = style_count, cols = 5, rows = 15:19, stack = TRUE)
  addStyle(wb, sheet = "Tab 3", style = createStyle(halign = "right"), cols = 6, rows = 15:19, stack = TRUE)
  
  writeData(wb, sheet = "Tab 3", 
            x = df_quarts,  
            startCol = 2, startRow = 15:19, headerStyle = style_date, colNames = FALSE)
  addStyle(wb, sheet = "Tab 3", style = style_date, cols = 2, rows = 15:19, stack = TRUE)
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}

