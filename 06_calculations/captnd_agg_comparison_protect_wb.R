

captnd_agg_comp_protect_wb <- function(wb){
  
  # Cover
  #protectWorksheet(wb, sheet = "Cover", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = TRUE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "Cover", style = createStyle(locked = FALSE),
  #cols = 2, rows = 11:16, 
  #stack = TRUE)
  #addStyle(wb, "Cover", style = createStyle(locked = FALSE),
  #cols = 1, rows = 1, 
  #stack = TRUE)
  
  # Tab 1
  #protectWorksheet(wb, sheet = "Referrals", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = FALSE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "Referrals", style = createStyle(locked = FALSE),
  #cols = 3, rows = 11, 
  #stack = TRUE)
  
  # Tab 1 Data
  sheetVisibility(wb)[4] <- "hidden"
  
  # Tab 2
  #protectWorksheet(wb, sheet = "DNAs", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = FALSE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "DNAs", style = createStyle(locked = FALSE),
  #cols = 3, rows = 11, 
  #stack = TRUE)
  
  # Tab 2 Data
  sheetVisibility(wb)[6] <- "hidden" # previously "veryHidden
  
  # Tab 3
  #protectWorksheet(wb, sheet = "Open cases", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = FALSE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "Open cases", style = createStyle(locked = FALSE),
  #cols = 3, rows = 11, 
  #stack = TRUE)
  
  # Tab 3 Data
  sheetVisibility(wb)[8] <- "hidden" # previously "veryHidden
  
  # Tab 4
  #protectWorksheet(wb, sheet = "First contact", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = FALSE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "First contact", style = createStyle(locked = FALSE),
  #cols = 3, rows = 11, 
  #stack = TRUE)
  
  # Tab 4 Data
  sheetVisibility(wb)[10] <- "hidden" # previously "veryHidden
  
  # Tab 5
  #protectWorksheet(wb, sheet = "Patients waiting", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = FALSE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "Patients waiting", style = createStyle(locked = FALSE),
  #cols = 3, rows = 11, 
  #stack = TRUE)
  
  # Tab 5 Data
  sheetVisibility(wb)[12] <- "hidden" # previously "veryHidden
  
  #Tab 6
  #protectWorksheet(wb, sheet = "Patients seen", protect = TRUE, lockFormattingCells = FALSE,
  #lockFormattingColumns = FALSE, lockInsertingColumns = TRUE,
  #lockDeletingColumns = TRUE, lockObjects = FALSE,
  #lockSelectingUnlockedCells = FALSE, lockSelectingLockedCells = TRUE,
  #lockAutoFilter = FALSE, password = password_strong)
  
  #addStyle(wb, "Patients seen", style = createStyle(locked = FALSE),
  #cols = 3, rows = 11, 
  #stack = TRUE)
  
  # Tab 6 Data
  sheetVisibility(wb)[14] <- "hidden" # previously "veryHidden
  
  
  # Lookups
  sheetVisibility(wb)[15] <- "hidden" # previously "veryHidden
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}