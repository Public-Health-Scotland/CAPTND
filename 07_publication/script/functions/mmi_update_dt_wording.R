
update_mmi_dt_wording <- function(wb, time_period){
  
  # Cover - B3 contains CAMHS/PT specific subtitle
  if (time_period == 'Quaterly') {
    writeData(wb, sheet = "Cover", 
            x = paste0(if_else(dataset_choice == "CAMHS", "CAMHS", "Psychological Therapies"), " Data Tables"),  
            startCol = 2, startRow = 3, headerStyle = style_text)
  addStyle(wb, sheet = "Cover", style = style_text, rows = 2, cols = 3)
  
  
  para_period <- paste0("This document summarises CAPTND data by quarter ending for the period ",
                        format(as.Date(month_start), "%B %Y") , " to ", 
                        format(as.Date(month_end), "%B %Y"), ".")
  
  writeData(wb, sheet = "Cover", 
            x = para_period,  
            startCol = 2, startRow = 5, headerStyle = style_text)
  addStyle(wb, sheet = "Cover", style = style_text,  cols = 2, rows = 5)
  
  
  
  # All chart tabs - tab title on (B2) 
  vec_tabs <- c("Tab 1", "Tab 2", "Tab 3")
  paras <- c(" referrals by health board name and quarter ending", 
             " referral acceptance status by health board name and quarter ending",
             " appointment attendance by health board name and quarter ending")
  
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], 
              x = paste0(if_else(dataset_choice == "CAMHS", 
                                 "CAMHS", "Psychological Therapies"), paras[i]), 
              startCol = 2, startRow = 2)
    addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11,
                                                  textDecoration = "bold"), rows = 2, cols = 2)
  }
  
  
  # All chart tabs - date range statement (B5)
  para_period <- paste0("CAPTND data by quarter, for the period ",
                        format(as.Date(month_start), "%B %Y") , " to ", 
                        format(as.Date(month_end), "%B %Y"), ".")
  
  vec_tabs <- c(#"Cover", 
    "Tab 1", "Tab 2", "Tab 3")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = para_period, startCol = 2, startRow = 3, headerStyle = style_text)
    addStyle(wb, vec_tabs[i], style = style_text, rows = 2, cols = 3)
  }
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
  } else {
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
    vec_tabs <- c("Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7")
    paras <- c(" referrals by health board name and month ending", 
               " referral acceptance status by health board name and month ending",
               " appointment attendance by health board name and month ending",
               " referral source by health board and month ending",
               " open cases by health board and month ending",
               " patients waiting by health board and month ending",
               " patients starting treatment by health board and month ending")
    
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], 
                x = paste0(if_else(dataset_choice == "CAMHS", 
                                   "CAMHS", "Psychological Therapies"), paras[i]), 
                startCol = 2, startRow = 4)
      addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11,
                                                    textDecoration = "bold"), rows = 2, cols = 4)
    }
    
    
    # All chart tabs - date range statement (B5)
    para_period <- paste0("CAPTND data by month, for the period ",
                          format(as.Date(month_start), "%B %Y") , " to ", 
                          format(as.Date(month_end), "%B %Y"), ".")
    
    vec_tabs <- c(#"Cover", 
      "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = para_period, startCol = 2, startRow = 5, headerStyle = style_text)
      addStyle(wb, vec_tabs[i], style = style_text, rows = 2, cols = 5)
    }
    
    # All chart tabs - MMI statement
    vec_tabs <- c(#"Cover", 
      "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = mmi_header, startCol = 2, startRow = 2, headerStyle = style_text)
      addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', 
                                                    fontSize = 11,
                                                    textDecoration = c("bold", "underline"), 
                                                    fontColour = "#E4080A"),rows = 2, cols = 2)
    }
    
    
    # save updates to GE - not sure if needed (leaving out for now)
    assign(x = "wb", value = wb, envir = .GlobalEnv)
    
  }
  
}

