
#######################.
### Update formulas ###
#######################.

# Author: Charlie Smith
# Date: 2025-03-04

# NB I struggled to loop this in a clever way, so ended up using quite a naive method.

update_formulas <- function(wb){
  
  # DQ Trend - table 1
  #for(j in 12:15){
    #for(k in 8:11){
      for(i in 20:34){
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 12, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec, 
                   startCol = 8,
                   startRow = i)
      
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 13, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec, 
                   startCol = 9,
                   startRow = i)
      
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 14, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec, 
                   startCol = 10,
                   startRow = i)
      
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 15, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec, 
                   startCol = 11,
                   startRow = i)
      }
    #}
  #}
  
  # DQ Trend - table 2
  #for(j in 8:11){
    for(i in 42:56){
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 8, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec,
                   startCol = 8,
                   startRow = i)
      
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 9, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec,
                   startCol = 9,
                   startRow = i)
      
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 10, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec,
                   startCol = 10,
                   startRow = i)
      
      formula_vec <- c(paste0("VLOOKUP($G", i, "&$C$12&$C$13&$C$14, 'Trend Data'!$A$2:$O$", trend_row, ",", 11, ", FALSE)"))
      writeFormula(wb, sheet = "DQ Trend", x = formula_vec,
                   startCol = 11,
                   startRow = i)
      
    }
  #}


  # DQ Trend Date - concat function (SLOWWWWW)
  # for(i in 2:trend_row){
  #   formula_vec <- c(paste0("CONCATENATE(", "B", i, "C", i, "D", i, "E", i, ")"))
  #   writeFormula(wb, sheet = "Trend Data", x = formula_vec,
  #                startCol = 1,
  #                startRow = i)
  # }
  
  # # purrr::walk()
  # spread_concat <- function(.x){
  #   
  #   formula_vec <- c(paste0("CONCATENATE(", "B", i, "C", i, "D", i, "E", i, ")"))
  #   writeFormula(wb, sheet = "Trend Data", x = formula_vec,
  #                startCol = 1,
  #                startRow = i)
  #   
  #   
  # }

  

  # # DQ Trend - Alt table
  #for(j in ){
    for(i in 18:63){
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",F$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 4,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",G$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 5,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",H$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 6,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",I$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 7,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",J$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 8,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",K$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 9,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",L$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 10,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",M$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 11,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",N$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 11,
                   startRow = i)
     
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",O$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 12,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",P$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 13,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",Q$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 14,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",R$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 15,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",S$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 16,
                   startRow = i)
      
      formula_vec <- c(paste0("=VLOOKUP($C$12&$C$13&$C", i,"&$C$14, 'Trend Data - Alt'!$A$2:$V$", trend_row_alt, ",T$11, FALSE)"))
      writeFormula(wb, sheet = "DQ Trend - Alt", x = formula_vec,
                   startCol = 17,
                   startRow = i)
      
       
    }
  #}
  
  
  
  
  
  
  
  # save updates to GE
  assign(x = "wb", value = wb, envir = .GlobalEnv)
  
  return(wb)
  
}



