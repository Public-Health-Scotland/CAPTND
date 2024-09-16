
##############################.
### Update DQ table values ###
##############################.

# Author: Charlie Smith
# Date: 2024-09-03

#save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_all")) 
# save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_latest"))


update_dq_values <- function(wb){
  
  # update submission summaries
  df_subs_sum <- read_parquet(paste0(pre_shorewise_output_dir, "/02_data_quality/submission_summary.parquet")) |> 
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
  
  df_subs_detail <- read_parquet(paste0(pre_shorewise_output_dir, "/02_data_quality/submission_detail.parquet")) |> 
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
  df_heat <- read_parquet(paste0(pre_shorewise_output_dir, "/02_data_quality/captnd_dq_clean_latest.parquet")) |> 
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
            startCol = 2, startRow = 13, #headerStyle = style_text, 
            colNames = TRUE, withFilter = TRUE, keepNA = TRUE, na.string = "-")
  
  addStyle(wb, sheet = "Heatmap Data", style = style_text, cols = 2:11, rows = 14:(nrow(df_heat)+14),
            stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_date, cols = 2, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = "Heatmap Data", style = style_count, cols = 9:10, rows = 14:(nrow(df_heat)+14),
           stack = TRUE, gridExpand = TRUE)

  # update vec_timeframe to "DQ Trend"
  # vec_timeframe
  writeData(wb, sheet = "DQ Trend", 
                 x = vec_timeframe,  
                 startCol = 7, startRow = 20, #headerStyle = style_text, 
                 colNames = TRUE, withFilter = TRUE)
  addStyle(wb, sheet = "DQ Trend", style = style_date, cols = 7, rows = 20:34, #41:(length(vec_timeframe)+41),
           stack = TRUE, gridExpand = TRUE)
  
  # update "Trend Data"
  df_trend <- read_parquet(paste0(pre_shorewise_output_dir, "/02_data_quality/captnd_dq_clean_all.parquet")) |> 
    mutate(value = str_to_title(value),
           value = factor(value, levels = c("Known", "Missing", "Invalid", "Not Known"))) |> 
    arrange(header_date_month, dataset_type, hb_name, variable, value) |> 
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
  
  deleteData(wb, sheet = "Trend Data", cols = 2:14, rows = 2:19681, gridExpand = TRUE)
  
  writeData(wb, sheet = "Trend Data",
            x = df_trend,
            startCol = 2, startRow = 2, #headerStyle = style_text,
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
  
  return(wb)
  
}