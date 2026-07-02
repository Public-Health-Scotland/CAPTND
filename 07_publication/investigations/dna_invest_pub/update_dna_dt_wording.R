#########################################.
### Update wording in DNA data tables ###.
#########################################.

# Author: Luke Taylor
# Date: 2026-07-02


update_dna_dt_wording <- function(wb){
  
  # Cover - B3 contains CAMHS/PT specific subtitle
  writeData(wb, sheet = "Cover", 
            x = paste0(if_else(dataset_choice == "CAMHS", "CAMHS", "Psychological Therapies"), " Data Tables"),  
            startCol = 2, startRow = 3, headerStyle = style_text)
  addStyle(wb, sheet = "Cover", style = style_text, rows = 2, cols = 3)
  
  
  para_period <- paste0("This document summarises CAPTND data for the period ",
                        format(as.Date(month_start), "%B %Y") , " to ", 
                        format(as.Date(month_end), "%B %Y"), ".")
  
  writeData(wb, sheet = "Cover", 
            x = para_period,  
            startCol = 2, startRow = 5, headerStyle = style_text)
  addStyle(wb, sheet = "Cover", style = style_text,  cols = 3, rows = 5)
  
  
  
  # All chart tabs - tab title on (B2) 
  vec_tabs <- c("Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", 
                "Tab 7", "Tab 8", "Tab 9", "Tab 10", "Tab 11")
  paras <- c(" total appointment crude DNA rate by biological sex, NHS Scotland", 
             " total appointment crude DNA rate by aggregated age group, NHS Scotland",
             " total appointment crude DNA rate by SIMD quintile, NHS Scotland",
             " total appointment age standardised DNA rate by SIMD quintile and biological sex, NHS Scotland",
             " total appointment age standardised DNA rate by urban rural classification and biological sex, NHS Scotland",
             " first contact appointment crude DNA rate by biological sex, NHS Scotland",
             " first contact appointment crude DNA rate by aggregated age group, NHS Scotland",
             " first contact appointment crude DNA rate by SIMD quintile, NHS Scotland",
             " first contact appointment age standardised DNA rate by SIMD quintile and biological sex, NHS Scotland",
             " first contact appointment age standardised DNA rate by urban rural classification and biological sex, NHS Scotland",
             " first contact appointment crude DNA rate by wait category, NHS Scotland")
  
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], 
              x = paste0(if_else(dataset_choice == "CAMHS", 
                                 "CAMHS", "Psychological Therapies"), paras[i]), 
              startCol = 2, startRow = 3)
    addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11,
                                                  textDecoration = "bold"), rows = 2, cols = 3)
  }
  
  
  # All chart tabs - date range statement (B5)
  para_period <- paste0("CAPTND data for the period ",
                        format(as.Date(month_start), "%B %Y") , " to ", 
                        format(as.Date(month_end), "%B %Y"), ".")
  
  vec_tabs <- c(#"Cover", 
    "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", 
    "Tab 7", "Tab 8", "Tab 9", "Tab 10", "Tab 11")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = para_period, startCol = 2, startRow = 5, headerStyle = style_text)
    addStyle(wb, vec_tabs[i], style = style_text, rows = 2, cols = 5)
    
  }
  
  
  #extract date
  ex_period <- paste0("Source: PHS CAPTND dataset extracted ",
                      format(as.Date(data_analysis_latest_date), "%Y-%m-%d"))
  
  vec_tabs <- c("Tab 1", "Tab 6")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 14, headerStyle = style_text)
    
  }
  
  
  vec_tabs <- c("Tab 2", "Tab 7", "Tab 11")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 16, headerStyle = style_text)
    
  }
  
  vec_tabs <- c("Tab 3", "Tab 8")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 17, headerStyle = style_text)
    
  }
  
  vec_tabs <- c("Tab 4", "Tab 9")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 18, headerStyle = style_text)
    
  }
  
  vec_tabs <- c("Tab 4", "Tab 9")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 30, headerStyle = style_text)
    
  }
  
  vec_tabs <- c("Tab 5", "Tab 10")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 21, headerStyle = style_text)
    
  }
  
  vec_tabs <- c("Tab 5", "Tab 10")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 36, headerStyle = style_text)
    
  }
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}



