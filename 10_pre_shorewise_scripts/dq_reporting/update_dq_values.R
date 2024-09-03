
##############################.
### Update DQ table values ###
##############################.

# Author: Charlie Smith
# Date: 2024-09-03

#save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_all")) 
# save_as_parquet(path = paste0(data_quality_report_dir, "/captnd_dq_clean_latest"))


update_dq_values <- function(wb){
  
  # update submission summaries
  df_subs_sum <- read_parquet(paste0(pre_shorewise_output_dir, "/02_data_quality/submission_summary.parquet"))
  
  writeData(wb, sheet = "HB Submissions", 
            x = df_subs_sum,  
            startCol = 2, startRow = 12, #headerStyle = style_text, 
            colNames = TRUE)
  addStyle(wb, sheet = "HB Submissions", style = style_count, cols = 3:5, rows = 12:14, 
           stack = TRUE, gridExpand = TRUE)
  
  
  df_subs_detail <- read_parquet(paste0(pre_shorewise_output_dir, "/02_data_quality/submission_detail.parquet"))
  
  writeData(wb, sheet = "HB Submissions", 
            x = df_subs_detail,  
            startCol = 8, startRow = 12, #headerStyle = style_text, 
            colNames = TRUE)
  addStyle(wb, sheet = "HB Submissions", style = style_count, cols = 8:11, rows = 12:41, 
           stack = TRUE, gridExpand = TRUE)
  
  
  # update "Heatmap Data"
  
  # update vec_timeframe to "DQ Trend"
  # vec_timeframe
  
  # update "Trend Data"
  
  # save updates to GE
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
  return(wb)
  
}