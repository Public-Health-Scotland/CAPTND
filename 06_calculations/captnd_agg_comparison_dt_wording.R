
captnd_agg_comp_dt_wording <- function(wb){
  
  # Cover - B3 contains CAMHS/PT specific subtitle
  writeData(wb, sheet = "Cover", 
              x = paste0(if_else(dataset_choice == "CAMHS", "CAMHS", "Psychological Therapies"), " Comparison Data Tables"),  
              startCol = 2, startRow = 3, headerStyle = style_text)
    addStyle(wb, sheet = "Cover", style = style_text, rows = 2, cols = 3)
    
    
    para_period <- paste0("This document summarises CAPTND and aggregate data by month end, for the period ",
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
    vec_tabs <- c("Referrals", "DNAs", "Open cases", "First contact", "Patients waiting", "Patients seen")
    paras <- c(" referrals by health board name and month ending", 
               " total appointment DNAs by health board name and month ending",
               " open cases by health board name and month ending",
               " first contacts by health board and month ending",
               " patients waiting for treatment by health board and month ending",
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
      "Referrals", "DNAs", "Open cases", "First contact", "Patients waiting", "Patients seen")
    for(i in 1:length(vec_tabs)){
      writeData(wb, vec_tabs[i], x = para_period, startCol = 2, startRow = 5, headerStyle = style_text)
      addStyle(wb, vec_tabs[i], style = style_text, rows = 2, cols = 5)
    }
    
    # All chart tabs - MMI statement
    vec_tabs <- c(#"Cover", 
      "Referrals", "DNAs", "Open cases", "First contact", "Patients waiting", "Patients seen")
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
  