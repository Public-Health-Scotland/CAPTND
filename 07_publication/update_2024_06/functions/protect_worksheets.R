
##########################.
### Protect Worksheets ###
##########################.

# Author: Charlie Smith
# Email: charlie.smith2@phs.scot
# Date: 2024-08-15

protect_worksheets <- function(wb){
  
  # Cover
  protectWorksheet(wb, sheet = "Cover", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = TRUE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Cover", style = createStyle(locked = FALSE),
           cols = 2, rows = 15:17, 
           stack = TRUE)
  addStyle(wb, "Cover", style = createStyle(locked = FALSE),
           cols = 1, rows = 1, 
           stack = TRUE)
  
  
  # Tab 1
  protectWorksheet(wb, sheet = "Tab 1", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockSelectingUnlockedCells = FALSE, 
                   lockSelectingLockedCells = TRUE, 
                   lockObjects = FALSE, # test
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 1", style = createStyle(locked = FALSE),
           cols = 3, rows = 11, 
           stack = TRUE)
  
  # addStyle(wb, "Tab 1", style = createStyle(locked = FALSE), # test - see if able to access alt text
  #          cols = 2:7, rows = 21:38, gridExpand = T,
  #          stack = TRUE)
  
  # Tab 1 Data
  sheetVisibility(wb)[3] <- "veryHidden"
  
  # Tab 2
  protectWorksheet(wb, sheet = "Tab 2", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
  
  addStyle(wb, "Tab 2", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
  
  # Tab 2 Data
  sheetVisibility(wb)[5] <- "veryHidden"
  
  
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
  sheetVisibility(wb)[7] <- "veryHidden"
  
  # Lookups
  sheetVisibility(wb)[8] <- "veryHidden"
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}