
##############################.
### Update DQ table values ###
##############################.

# Author: Charlie Smith
# Date: 2024-09-03

#save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_all")) 
# save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_latest"))


update_dq_values <- function(wb){
  
  # update submission summaries
  df_subs_sum <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/submission_summary.parquet")) |> 
    rename(`Dataset Type` = dataset_type, 
           `Proportion (%)` = Proportion)
  
  writeDataTable(wb, sheet = "HB Submissions", 
            x = df_subs_sum,  
            startCol = 2, startRow = 12, #headerStyle = style_text, 
            colNames = TRUE, withFilter = FALSE)
  addStyle(wb, sheet = "HB Submissions", style = style_text, cols = 2:3, rows = 12:14, 
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "HB Submissions", style = style_count, cols = 4:6, rows = 12:14, 
           stack = TRUE, gridExpand = TRUE)
  
  df_subs_detail <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/submission_detail.parquet")) |> 
    rename(`Dataset Type` = dataset_type,
           `Health Board`= hb_name,
           PMS = pms)
  
  writeDataTable(wb, sheet = "HB Submissions", 
            x = df_subs_detail,  
            startCol = 8, startRow = 12, #headerStyle = style_text, 
            colNames = TRUE, withFilter = FALSE)
  addStyle(wb, sheet = "HB Submissions", style = style_text, cols = 8:11, rows = 12:41, 
           stack = TRUE, gridExpand = TRUE)
  
  
  # update "Heatmap Data"
  df_heat <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_latest.parquet")) |> 
    mutate(value = factor(value, levels = vec_value)) |> 
    append_nhsscotland_label_factor() |> 
    arrange(dataset_type, hb_name, variable, value) |> 
    rename(Month = header_date_month, 
           `Submission Status` = submission_status, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           PMS = pms,
           Variable = variable,
           `DQ Assessment` = value,
           Count = count, Total = total, Proportion = proportion)
  
  writeDataTable(wb, sheet = "Heatmap Data", 
            x = df_heat,  
            startCol = 2, startRow = 13, headerStyle = style_text, 
            colNames = TRUE, withFilter = TRUE, keepNA = TRUE, na.string = "-")
  
  addStyle(wb, sheet = "Heatmap Data", style = style_text, cols = 2:11, rows = 14:(nrow(df_heat)+14),
            stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_date, cols = 2, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_count, cols = 9:10, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_percent, cols = 11, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)

  # update vec_timeframe to "DQ Trend"
  # vec_timeframe
  writeData(wb, sheet = "DQ Trend", 
                 x = vec_timeframe,  
                 startCol = 7, startRow = 20, #headerStyle = style_text, 
                 colNames = TRUE, withFilter = TRUE)
  addStyle(wb, sheet = "DQ Trend", style = style_date, cols = 7, rows = 20:34, #41:(length(vec_timeframe)+41),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "DQ Trend", style = style_percent2, cols = 8:11, rows = 20:34, #41:(length(vec_timeframe)+41),
           stack = TRUE, gridExpand = TRUE)
  
  # update "Trend Data"
  df_trend <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_all.parquet")) |> 
    mutate(value = str_to_title(value),
           value = factor(value, levels = c("Known", "Missing", "Invalid", "Not Known"))) |> 
    arrange(header_date_month, dataset_type, hb_name, variable, value) |> 
    append_nhsscotland_label_factor() |> 
    select(Month = header_date_month, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           Variable = variable,
           `Submission Status` = submission_status, 
           PMS = pms,
           `DQ Assessment` = value,
           Count = count, -total, Proportion = proportion) |> 
    pivot_wider(names_from = `DQ Assessment`, values_from = c(Count, Proportion),
                names_sep = " ")
  
  trend_row <- nrow(df_trend)+1
  
  deleteData(wb, sheet = "Trend Data", cols = 2:14, rows = 2:trend_row, gridExpand = TRUE)
  
  writeData(wb, sheet = "Trend Data",
            x = df_trend,
            startCol = 2, startRow = 2, #headerStyle = style_text,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  # update concat depending on number of rows
  # for(i in 2:trend_row){
  #   formula = paste0("=CONCATENATE(B", i, ", C", i, ", D", i, ", G", i,")")
  #   writeFormula(wb, sheet = "Trend Data", x = formula, startCol = 1, startRow = i, array = FALSE)
  # }
  
  # trend - alternative table with tic marks
  df_trend2 <- read_parquet(paste0(pre_shorewise_output_dir, "/02_dq_report_files/captnd_dq_clean_all.parquet")) |> 
    mutate(value = str_to_title(value),
           value = factor(value, levels = c("Known", "Missing", "Invalid", "Not Known"))) |> 
    append_nhsscotland_label_factor() |> 
    arrange(dataset_type, hb_name, header_date_month) |> 
    select(Month = header_date_month, 
           Dataset = dataset_type, 
           `Health Board` = hb_name, 
           Variable = variable,
           `Submission Status` = submission_status, 
           PMS = pms,
           `DQ Assessment` = value,
           -count, -total, Proportion = proportion) |> 
    mutate(Month =  format(as.Date(Month), "%b-%Y")) |> 
    pivot_wider(names_from = Month, values_from = Proportion)
  
  df_trend2_dates <- data.frame(dates = df_trend2 |> select(7:21) |> colnames()) |> 
    pivot_wider(names_from = dates, values_from = dates) 
  
  # export(df_trend2, 
  #        file = paste0(pre_shorewise_output_dir, "/02_data_quality/captnd_dq_trend_summary.xlsx"),
  #        format = "xlsx")
  
  trend_row_alt <- nrow(df_trend2)+1
  
  deleteData(wb, sheet = "Trend Data - Alt", cols = 2:21, rows = 1:trend_row_alt, gridExpand = TRUE)
  
  writeData(wb, sheet = "Trend Data - Alt",
            x = df_trend2,
            startCol = 2, startRow = 1, #headerStyle = style_text,
            colNames = TRUE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  writeData(wb, sheet = "DQ Trend - Alt",
            x = df_trend2_dates,
            startCol = 4, startRow = 17, headerStyle = style_header,
            colNames = FALSE, withFilter = FALSE,  keepNA = TRUE, na.string = "-")
  
  
  # update references
  vec_hb <- unique(df_trend$`Health Board`)
  writeData(wb, sheet = "Refs", 
            x = vec_hb,  
            startCol = 3, startRow = 3, #headerStyle = style_text, 
            colNames = FALSE, withFilter = FALSE)
  
  vec_vars <- unique(df_trend$Variable)
  writeData(wb, sheet = "Refs", 
            x = vec_vars,  
            startCol = 4, startRow = 3, #headerStyle = style_text, 
            colNames = FALSE, withFilter = FALSE)
  
  # save updates to GE
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  assign(x = "trend_row", value = trend_row, envir = .GlobalEnv)
  assign(x = "trend_row_alt", value = trend_row_alt, envir = .GlobalEnv)
  
  return(wb)
  
}
