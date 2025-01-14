
###########################.
# Add HQ heatmap images ###
###########################.

# Author: Charlie Smith
# Date: 2024-08-30

add_dq_heatmaps <- function(wb){
  
  insertImage(wb, "Known Records", paste0(data_quality_report_dir, "/known_update.png"), 
              startRow = 10, startCol = 2, width = 12, height = 9, units = "in")
  
  
  insertImage(wb, "Not Known Records", paste0(data_quality_report_dir, "/not_known_update.png"), 
              startRow = 10, startCol = 2, width = 12, height = 9, units = "in")
  
  
  insertImage(wb, "Missing Records", paste0(data_quality_report_dir, "/missing_update.png"), 
              startRow = 10, startCol = 2, width = 12, height = 9, units = "in")
  
  
  insertImage(wb, "Invalid Records", paste0(data_quality_report_dir, "/invalid_update.png"), 
              startRow = 10, startCol = 2, width = 12, height = 9, units = "in")
  
  # save updates to GE
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}

