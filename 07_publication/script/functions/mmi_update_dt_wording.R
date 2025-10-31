
update_mmi_dt_wording <- function(wb, time_period){
  
    writeData(wb, sheet = "Cover", 
              x = paste0(if_else(dataset_choice == "CAMHS", "CAMHS", "Psychological Therapies"), " Data Tables"),  
              startCol = 2, startRow = 3, headerStyle = style_text)
    addStyle(wb, sheet = "Cover", style = style_text, rows = 2, cols = 3)
    
    
    para_period <- paste0("This document summarises CAPTND data by month ending for the period ",
                          format(as.Date(month_start), "%B %Y") , " to ", 
                          format(as.Date(month_end), "%B %Y"), ".")
    
    writeData(wb, sheet = "Cover", 
              x = para_period,  
              startCol = 2, startRow = 7, headerStyle = style_text)
    addStyle(wb, sheet = "Cover", style = style_text,  cols = 2, rows = 7)
    
    mmi_header <- "Management Information - Not For Onward Release"
    
    writeData(wb, sheet = "Cover", 
              x = mmi_header,  
              startCol = 2, startRow = 5, headerStyle = style_text)
    addStyle(wb, sheet = "Cover", style = createStyle(fontName = 'Arial', 
                                                      fontSize = 11,
                                                      textDecoration = c("bold", "underline"), 
                                                      fontColour = "#E4080A"),rows = 5, cols = 2)
    
    
    
    # All chart tabs - tab title on (B4) 
    vec_tabs <- c("Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 6", "Tab 7",
                  "Tab 8", "Tab 10")
    paras <- c(" referrals by health board of treatment and biological sex, month ending", 
               " referrals by health board of treatment and age group, month ending",
               " referrals by health board of treatment and SIMD quintile, month ending",
               " referral status by health board of treatment, month ending",
               " top five referral sources by health board of treatment, month ending",
               " first contact attendance by health board of treatment, month ending",
               " total appointment DNAs by health board of treament, month ending",
               " first contact DNA rate by health board of treatment and SIMD quintile, month ending")
    
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], 
                x = paste0(if_else(dataset_choice == "CAMHS", 
                                   "CAMHS", "Psychological Therapies"), paras[i]), 
                startCol = 2, startRow = 3)
      addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11,
                                                    textDecoration = "bold"), rows = 2, cols = 4)
    }
    
    
    # All chart tabs - date range statement (B5)
    para_period <- paste0("CAPTND data by month, for the period ",
                          format(as.Date(month_start), "%B %Y") , " to ", 
                          format(as.Date(month_end), "%B %Y"), ".")
    
    vec_tabs <- c(#"Cover", 
      "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7", 
      "Tab 8", "Tab 9", "Tab 10", "Tab 11")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = para_period, startCol = 2, startRow = 5, headerStyle = style_text)
      addStyle(wb, vec_tabs[i], style = style_text, rows = 2, cols = 5)
    }
    
    # All chart tabs - MMI statement
    vec_tabs <- c(#"Cover", 
      "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7", 
      "Tab 8", "Tab 9", "Tab 10", "Tab 11")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = mmi_header, startCol = 2, startRow = 2, headerStyle = style_text)
      addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', 
                                                    fontSize = 11,
                                                    textDecoration = c("bold", "underline"), 
                                                    fontColour = "#E4080A"),rows = 2, cols = 2)
    }
    
    
    #extract date
    ex_period <- paste0("Source: PHS CAPTND dataset extracted ",
                        format(as.Date(data_analysis_latest_date), "%Y-%m-%d") , ";",
                        " NRS population estimates published 2024-11-21.")
    
    
    vec_tabs <- c("Tab 5", "Tab 9")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 23, headerStyle = style_text)
      
    }
    
    
    vec_tabs <- c("Tab 4", "Tab 7")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 30, headerStyle = style_text)
      
    }
    
    
    vec_tabs <- c("Tab 3", "Tab 10", "Tab 11")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 21, headerStyle = style_text)
      
    }
    
    writeData(wb, sheet = "Tab 1", 
              x = ex_period,  
              startCol = 2, startRow = 18, headerStyle = style_text)
    
    writeData(wb, sheet = "Tab 8", 
              x = ex_period,  
              startCol = 2, startRow = 29, headerStyle = style_text)
    
    writeData(wb, sheet = "Tab 2", 
              x = ex_period,  
              startCol = 2, startRow = 20, headerStyle = style_text)
    
    writeData(wb, sheet = "Tab 6", 
              x = ex_period,  
              startCol = 2, startRow = 22, headerStyle = style_text)
    
    
    # save updates to GE - not sure if needed (leaving out for now)
    assign(x = "wb", value = wb, envir = .GlobalEnv)
    
  
}

