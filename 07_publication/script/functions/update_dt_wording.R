
#####################################.
### Update wording in data tables ###
#####################################.

# Author: Charlie Smith
# Date: 2024-08-08
#Updated: 2024-12-30

update_dt_wording <- function(wb){
  
  # Cover - B3 contains CAMHS/PT specific subtitle
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
  addStyle(wb, sheet = "Cover", style = style_text,  cols = 3, rows = 5)
  
  
  
  # All chart tabs - tab title on (B2) 
  vec_tabs <- c("Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7", "Tab 8", "Tab 9", "Tab 10", "Tab 11")
  paras <- c(" referrals by health board of treatment and biological sex, quarter ending", 
             " referrals by health board of treatment and age group, quarter ending",
             " referrals by health board of treatment and SIMD quintile, quarter ending",
             " referral status by health board of treatment, quarter ending",
             " top five referral non-acceptance reasons by health board of treatment, quarter ending",
             " top five actions following referral non-acceptance by health board of treatment, quarter ending",
             " top five referral sources by health board of treatment, quarter ending",
             " first contact attendance by health board of treatment, quarter ending",
             " total appointment DNAs by health board of treament, quarter ending",
             " top five appointment care locations by health board of treatment, quarter ending",
             " top five professional groups conducting appointments by health board of treatment, quarter ending")
  
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], 
              x = paste0(if_else(dataset_choice == "CAMHS", 
                                 "CAMHS", "Psychological Therapies"), paras[i]), 
              startCol = 2, startRow = 3)
    addStyle(wb, vec_tabs[i], style = createStyle(fontName = 'Arial', fontSize = 11,
                                                  textDecoration = "bold"), rows = 2, cols = 3)
  }
  
  
  # All chart tabs - date range statement (B5)
  para_period <- paste0("CAPTND data by quarter, for the period ",
                        format(as.Date(month_start), "%B %Y") , " to ", 
                        format(as.Date(month_end), "%B %Y"), ".")
  
  vec_tabs <- c(#"Cover", 
    "Tab 1", "Tab 2", "Tab 3", "Tab 4", "Tab 5", "Tab 6", "Tab 7", "Tab 8", "Tab 9", "Tab 10", "Tab 11")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = para_period, startCol = 2, startRow = 5, headerStyle = style_text)
    addStyle(wb, vec_tabs[i], style = style_text, rows = 2, cols = 5)
  
  }
  
  
  #extract date
  ex_period <- paste0("Source: PHS CAPTND dataset extracted ",
                        format(as.Date(data_analysis_latest_date), "%Y-%m-%d") , ".")
  
  vec_tabs <- c("Tab 5", "Tab 6", "Tab 7", "Tab 10", "Tab 11")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 22, headerStyle = style_text)
    
  }
  
  
  vec_tabs <- c("Tab 2", "Tab 4", "Tab 8")
  for(i in 1:length(vec_tabs)){
    writeData(wb, vec_tabs[i], x = ex_period, startCol = 2, startRow = 20, headerStyle = style_text)
    
  }
  
  writeData(wb, sheet = "Tab 1", 
            x = ex_period,  
            startCol = 2, startRow = 18, headerStyle = style_text)
  
  writeData(wb, sheet = "Tab 3", 
            x = ex_period,  
            startCol = 2, startRow = 21, headerStyle = style_text)
  
  writeData(wb, sheet = "Tab 9", 
            x = ex_period,  
            startCol = 2, startRow = 19, headerStyle = style_text)
  
  
  # save updates to GE - not sure if needed (leaving out for now)
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
}
