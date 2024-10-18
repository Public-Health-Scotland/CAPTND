##########################.
### Protect Worksheets ###
##########################.

# Author: Luke Taylor
# Date: 2024-09-30

protect_mmi_worksheets <- function(wb, time_period){
  if(time_period == "Quarterly"){
  
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
  
  # Tab 1 Data
  sheetVisibility(wb)[3] <- "hidden"
  
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
  sheetVisibility(wb)[5] <- "hidden" # previously "veryHidden
  
  
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
  
  # Lookups
  sheetVisibility(wb)[8] <- "hidden" # previously "veryHidden
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
  }
  
  else{
    
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
    
    # Tab 1 Data
    sheetVisibility(wb)[2] <- "hidden"
    
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
    sheetVisibility(wb)[5] <- "hidden" # previously "veryHidden
    
    
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
           cols = 3, rows = 11, 
           stack = TRUE)
    
    # Tab 4 Data
    sheetVisibility(wb)[9] <- "hidden" # previously "veryHidden
    
    #Tab 5
    protectWorksheet(wb, sheet = "Tab 5", protect = TRUE, lockFormattingCells = FALSE,
                   lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
                   lockDeletingColumns = TRUE, lockObjects = FALSE,
                   lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
                   lockAutoFilter = FALSE, password = password_strong)
    
    addStyle(wb, "Tab 5", style = createStyle(locked = FALSE),
           cols = 3, rows = 11:12, 
           stack = TRUE)
    
    # Tab 5 Data
    sheetVisibility(wb)[11] <- "hidden" # previously "veryHidden
    
    #Tab 6
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
    
    # Lookups
    sheetVisibility(wb)[14] <- "hidden" # previously "veryHidden
    
    # save updates to GE - not sure if needed (leaving out for now)
    assign(x = "wb", value = wb, envir = .GlobalEnv)
  
  }
  
}


