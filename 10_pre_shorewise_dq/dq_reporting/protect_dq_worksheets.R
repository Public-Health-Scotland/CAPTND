
#############################.
### Protect DQ Worksheets ###
#############################.

# Author: Charlie Smith
# Date: 2024-09-06

protect_dq_worksheets <- function(wb){
  
  protectWorksheet(wb, sheet = "READ ME", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  protectWorksheet(wb, sheet = "Data Definitions", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  protectWorksheet(wb, sheet = "HB Submissions", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  protectWorksheet(wb, sheet = "Heatmap Data", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Heatmap Data", style = createStyle(locked = FALSE),
           cols = 2:11, rows = 13, stack = TRUE)
  
  
  protectWorksheet(wb, sheet = "Known Records", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  protectWorksheet(wb, sheet = "Not Known Records", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  protectWorksheet(wb, sheet = "Missing Records", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  protectWorksheet(wb, sheet = "Invalid Records", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  
  protectWorksheet(wb, sheet = "DQ Trend", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "DQ Trend", style = createStyle(locked = FALSE),
           cols = 3, rows = 12:14, stack = TRUE)
  
  protectWorksheet(wb, sheet = "DQ Trend - Alt", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "DQ Trend - Alt", style = createStyle(locked = FALSE),
           cols = 3, rows = 12:15, stack = TRUE)
  
  protectWorksheet(wb, sheet = "Analyst Checks", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "DQ Trend - Alt", style = createStyle(locked = FALSE),
           cols = 38, rows = 15, stack = TRUE)
  
  # Hide data tabs - if these break, check the developer tab of the output workbook for tab index
  # trend data
  sheetVisibility(wb)[11] <- "veryHidden" # previously "veryHidden
  
  # trend data - alt 
  sheetVisibility(wb)[13] <- "veryHidden" # previously "veryHidden
  
  # refs
  sheetVisibility(wb)[14] <- "veryHidden" # previously "veryHidden
  
  # Analyst Checks
  sheetVisibility(wb)[15] <- "veryHidden" # previously "veryHidden
  
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}

