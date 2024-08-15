
################################.
### Update data table values ###
################################.

# Author: Charlie Smith
# Date: 2024-08-13

update_dt_values <- function(wb){
  
  # get quarters ending for all dts
  df_quarts <- read_parquet(paste0(ref_dir, "referrals_quarter_hb.parquet")) |> 
    ungroup() |> select(quarter_ending) |> unique() #|> pull()
  

  
  # based on dataset_choice...
  
  #df_qt_ds_hb <- df_ds_hb_name |> cross_join(df_quarts)
  
  
  # quarterly referrals by HB
  df_refs <- read_parquet(paste0(ref_dir, "referrals_quarter_hb.parquet")) |> 
    # right_join(df_qt_ds_hb, by = c("quarter_ending", "dataset_type", "hb_name")) |> 
    # mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    # arrange(dataset_type, hb_name) |> 
    # rename(`Health board` = hb_name) |> 
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
  df_acc_status <- read_parquet(paste0(non_acc_dir, "non_acceptance_summary_quarter_hb.parquet")) |> 
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
  first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |> 
    filter(dataset_type == dataset_choice) 
  
  writeData(wb, sheet = "Tab 3 Data", 
            x = first_att_latest,  
            startCol = 2, startRow = 2, headerStyle = style_text)
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

