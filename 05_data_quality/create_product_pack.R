library(openxlsx)


#get most recent and earliest dates for which products have been created 

  files <- list.files(product2_dir) # list files in directory
  files <- files[grep("heatmap", files)] # gets only the heatmap file names
  files <- gsub("product2_heatmap_","", gsub(".png","", files)) # remove the non-date elements of the file names
  
  count_files <- length(files) # finds number of files
  
  latest_date <- files[count_files] # creates object with latest product date only
  earliest_date <- files[1] # creates object with earliest product date only


# pull template excel workbook containing products 1, 2, 3 - narrative and readme in template

wb <- loadWorkbook(paste0("../../../output/product_pack_working/template_products.xlsx"))

# load in the created .pngs of each product

insertImage(wb, "Data Retention", paste0(product1_dir, "/product1.png"),
            startRow = 18, startCol = 2, width = 29, height = 13.5, units = "cm")

insertImage(wb, "RTT summary", paste0(product2_dir, "/product2_heatmap_", latest_date, ".png"), #insert date conditional
            startRow = 21, startCol = 2, width = 24, height = 13.5, units = "cm")
insertImage(wb, "RTT summary", paste0(product2_dir, "/product2_heatmap_", earliest_date, ".png"), #insert date conditional
            startRow = 37, startCol = 2, width = 24, height = 13.5, units = "cm")

insertImage(wb, "Data Completion", paste0(product3_dir, "/product3_closed_cases_until_", latest_date, ".png"), #insert date conditional
            startRow = 16, startCol = 2, width = 30, height = 15, units = "cm")

# # If wanting to add a dated comment above main narrative
# prod1_narrative <- paste0("Product 1 test ", latest_date)
# prod2_narrative <- paste0("Product 2 test ", latest_date)
# prod3_narrative <- paste0("Product 3 test ", latest_date)
# 
# # insert narrative text to each sheet
# writeData(wb, "Data Retention", x = prod1_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
# writeData(wb, "RTT summary", x = prod2_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
# writeData(wb, "Data Completion", x = prod3_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize


saveWorkbook(wb, paste0(external_reports_dir, "/Product_Pack_", latest_date, ".xlsx"), overwrite = TRUE)








