
###########################.
### Create Product Pack ###
###########################.

# Author: Bex Madden
# Date: 2024-03-07

library(openxlsx)


# 1. Derive dates ---------------------------------------------------------

#get most recent and earliest dates for which products have been created 

  files <- list.files(product2_dir) # list files in directory
  files <- files[grep("heatmap", files)] # gets only the heatmap file names
  files <- gsub("qt_product2_heatmap_","", gsub(".png","", files)) # remove the non-date elements of the file names
  
  count_files <- length(files) # finds number of files
  
  latest_date <- files[count_files] # creates object with latest product date only
  earliest_date <- files[1] # creates object with earliest product date only



# 2. Populate workbook ---------------------------------------------------------


# pull template excel workbook containing products 1, 2, 3 - narrative and readme in template

wb <- loadWorkbook(paste0("../../../output/product_pack_working/template_products.xlsx"))

# load in the created .pngs of each product

insertImage(wb, "1. Data Retention", paste0(product1_dir, "/product1.png"),
            startRow = 11, startCol = 2, width = 29, height = 13.5, units = "cm")

insertImage(wb, "2. RTT Summary", paste0(product2_dir, "/qt_product2_heatmap_", latest_date, ".png"), 
            startRow = 10, startCol = 2, width = 29, height = 13.5, units = "cm")
# insertImage(wb, "2. RTT Summary", paste0(product2_dir, "/product2_heatmap_", earliest_date, ".png"), 
#             startRow = 31, startCol = 2, width = 24, height = 13.5, units = "cm")
# 
# insertImage(wb, "3. Data Completeness", paste0(product3_dir, "/product3_closed_cases_until_", latest_date, ".png"), 
#             startRow = 8, startCol = 2, width = 30, height = 15, units = "cm")

# If wanting to add a dated comment above main narrative
prod1_narrative <- paste0("The following heatmap shows retained data by Health Board in the 12 months prior to ", latest_date, ".")
prod2_narrative <- paste0("The following heatmap shows the percentage of the valid patient pathways where it is possible to calculate Unadjusted RTT,",
                          " by Health Board in the 4 quarters prior to ", latest_date, ".")
#prod3_narrative <- paste0("The data shown relates to completed patient pathways in CAPTND up until ", latest_date)
 
# insert narrative text to each sheet
writeData(wb, "1. Data Retention", x = prod1_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
writeData(wb, "2. RTT Summary", x = prod2_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
#writeData(wb, "3. Data Completeness", x = prod3_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize


saveWorkbook(wb, paste0(external_reports_dir, "/CAPTND_opti_summary_", latest_date, ".xlsx"), overwrite = TRUE)


