##########################.
### Protect Worksheets ###
##########################.

# Author: Luke Taylor
# Date: 2024-09-30

protect_mmi_worksheets_comp <- function(wb, time_period, dataset_choice){
  
  #Tab 16
  sheetVisibility(wb)[32] <- ifelse(dataset_choice == "CAMHS", "veryHidden", "visible")
  
  #Tab 16 data
  #sheetVisibility(wb)[33] <- ifelse(dataset_choice == "CAMHS", "veryHidden", "visible")
  
  wb
  
}

