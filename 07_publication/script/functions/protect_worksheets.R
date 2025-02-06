
##########################.
### Protect Worksheets ###
##########################.

# Author: Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2024-08-15
# Updated: 2024-12-30

protect_worksheets <- function(wb){
  
  # Cover
  protectWorksheet(wb, sheet = "Cover", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = NULL)
  
  addStyle(wb, "Cover", style = createStyle(locked = FALSE),
           cols = 2, rows = 13:21, 
           stack = TRUE)
  addStyle(wb, "Cover", style = createStyle(locked = FALSE),
           cols = 1, rows = 1, 
           stack = TRUE)
  
  
  # Tab 1
  protectWorksheet(wb, sheet = "Tab 1", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = NULL)
  
  addStyle(wb, "Tab 1", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 1 Data
  sheetVisibility(wb)[3] <- "hidden"
  
  # Tab 2
  protectWorksheet(wb, sheet = "Tab 2", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = NULL)
  
  
  addStyle(wb, "Tab 2", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  addStyle(wb, "Tab 2", style = createStyle(locked = FALSE), cols = 2, rows = 23, stack = TRUE)
  addStyle(wb, "Tab 2", style = createStyle(locked = FALSE), cols = 5, rows = 14, stack = TRUE)
  
  # Tab 2 Data
  sheetVisibility(wb)[5] <- "hidden"
  
  # Tab 3
  protectWorksheet(wb, sheet = "Tab 3", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 3", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 3 Data
  sheetVisibility(wb)[7] <- "hidden" # previously "veryHidden
  
  # Tab 4
  protectWorksheet(wb, sheet = "Tab 4", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 4", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 4 Data
  sheetVisibility(wb)[9] <- "hidden" # previously "veryHidden
  
  # Tab 5
  protectWorksheet(wb, sheet = "Tab 5", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 5", style = createStyle(locked = FALSE),cols = 2, rows = 9, stack = TRUE)
  addStyle(wb, "Tab 5", style = createStyle(locked = FALSE),cols = 3, rows = 11:12, stack = TRUE)
  addStyle(wb, "Tab 5", style = createStyle(locked = FALSE),cols = 2, rows = 24:25, stack = TRUE)
  
  # Tab 5 Data
  sheetVisibility(wb)[11] <- "hidden" # previously "veryHidden
  
  # Tab 6
  protectWorksheet(wb, sheet = "Tab 6", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 6", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 6 Data
  sheetVisibility(wb)[13] <- "hidden" # previously "veryHidden
  
  # Tab 7
  protectWorksheet(wb, sheet = "Tab 7", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 7", style = createStyle(locked = FALSE),
           cols = 3, rows = 11, 
           stack = TRUE)
  
  # Tab 7 Data
  sheetVisibility(wb)[15] <- "hidden" # previously "veryHidden
  
  # Tab 8
  protectWorksheet(wb, sheet = "Tab 8", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 8", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 8 Data
  sheetVisibility(wb)[17] <- "hidden" # previously "veryHidden
  
  # Tab 9
  protectWorksheet(wb, sheet = "Tab 9", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 9", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 9 Data
  sheetVisibility(wb)[19] <- "hidden" # previously "veryHidden
  
  
  # Lookups
  sheetVisibility(wb)[20] <- "hidden" # previously "veryHidden
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}