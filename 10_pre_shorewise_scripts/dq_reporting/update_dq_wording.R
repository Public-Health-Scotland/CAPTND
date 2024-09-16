
#####################################.
### Update wording in data tables ###
#####################################.

# Author: Charlie Smith
# Date: 2024-08-30

update_dq_wording <- function(wb){
  
  # time range statement
  vec_tabs <- c("READ ME", "Data Definitions")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], 
              x = paste0("This document summarises the quality of CAPTND data submissions from ",
                         month_word_start, " to ", month_word_end, "."), 
              startCol = 2, startRow = 6, headerStyle = createStyle(fontName = 'Arial', fontSize = 11))
    addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11), cols = 2, rows = 6, stack = TRUE)
  }
  
  # add latest month timeframe statement
  para0_snapshot <- paste0("This tab summarises the quality of CAPTND data submissions from ",
                           month_word_end, ".")
  
  vec_tabs <- c("HB Submissions", "Heatmap Data", "Known Records", "Not Known Records", 
                "Missing Records", "Invalid Records")
  
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = para0_snapshot, startCol = 2, startRow = 6, headerStyle = style_text)
    addStyle(wb, vec_tabs[i], style = style_text, cols = 2, rows = 6)
  }
  
  # trend timeframe
  writeData(wb, "DQ Trend", 
            x = paste0("This tab summarises the quality of CAPTND data submissions from ",
                       month_word_start, " to ", month_word_end, "."), 
            startCol = 2, startRow = 6, headerStyle = style_text)
  addStyle(wb, "DQ Trend", style = style_text, cols = 2, rows = 6)
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}