
###########################.
### Create Product Pack ###
###########################.

# Author: Bex Madden
# Date: 2024-03-07


create_product_pack_mth <- function(){
library(openxlsx)


# 1. Derive dates ---------------------------------------------------------

#get most recent and earliest dates for which products have been created 

  files <- list.files(opti_report_dir) # list files in directory
  files <- files[grep("mth_product2_heatmap_rework_", files)] # gets only the heatmap file names
  files <- gsub("mth_product2_heatmap_rework_","", gsub(".png","", files)) # remove the non-date elements of the file names
  
  count_files <- length(files) # finds number of files
  
  latest_date <- files[count_files] # creates object with latest product date only
  earliest_date <- files[1] # creates object with earliest product date only



# 2. Populate workbook ---------------------------------------------------------


# pull template excel workbook containing products 1, 2, 3 - narrative and readme in template


wb <- loadWorkbook("../../../report_templates/product_pack/template_products_rework.xlsx")

modifyBaseFont(wb, fontName = "Arial")

# load in the created .pngs of each product

insertImage(wb, "1. Data Key Completeness", paste0(opti_report_dir, "/product1.png"),
            startRow = 10, startCol = 2, width = 29, height = 13.5, units = "cm")

insertImage(wb, "2. Data Retention", paste0(opti_report_dir, "/mth_product4_heatmap_", latest_date, ".png"),
            startRow = 10, startCol = 2, width = 29, height = 13.5, units = "cm")

insertImage(wb, "3. RTT Summary", paste0(opti_report_dir, "/mth_product2_heatmap_rework_", latest_date, ".png"), 
            startRow = 10, startCol = 2, width = 29, height = 13.5, units = "cm")
# 
# insertImage(wb, "3. Data Completeness", paste0(product3_dir, "/product3_closed_cases_until_", latest_date, ".png"), 
#             startRow = 8, startCol = 2, width = 30, height = 15, units = "cm")

# If wanting to add a dated comment above main narrative
prod1_narrative <- paste0("The following heatmap shows the percentage of records submitted with valid data keys by health board in the 12 months up to and including ", latest_date, ".")
prod4_narrative <- paste0("The following heatmap shows the percentage of submitted records that are retained for further analyses by health board in the 12 months",
                          " up to and including ", latest_date, ".")
prod2_narrative <- paste0("The following heatmap shows the percentage of retained patient pathways where we believe treatment",
                          " has started for which it is possible to calculate unadjusted RTT. This is shown by health board in the",
                          " 12 months up to and including ", latest_date, ". Only pathways for which either a referral or appointment",
                          " record were recieved during the submission month have been assessed.",
                          " See the table below this heatmap for detail on the reasons RTT cannot be calculated in the latest month.")
prod2_narrative2 <- paste0("The following table contains detail on the reasons why RTT could not be calculated in the latest month.",
                           " Please use the drop-down column headers to find the data relating to your own health board.")
#prod3_narrative <- paste0("The data shown relates to completed patient pathways in CAPTND up until ", latest_date)
 
# insert narrative text to each sheet
writeData(wb, "1. Data Key Completeness", x = prod1_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
writeData(wb, "2. Data Retention", x = prod4_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
writeData(wb, "3. RTT Summary", x = prod2_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize
writeData(wb, "3. RTT Summary", x = prod2_narrative2, startCol = 2, startRow = 26) #, headerStyle = my_fontsize
#writeData(wb, "3. Data Completeness", x = prod3_narrative, startCol = 2, startRow = 6) #, headerStyle = my_fontsize


# read in data relating to ability to calculate RTT 

source("./05_data_quality/get_prod2_reasons_table.R")
p2_reasons <- get_prod2_reasons_table(latest_date = latest_date)

writeDataTable(wb, "3. RTT Summary", p2_reasons, 
               startRow = 28, startCol = 2,
               tableStyle = "TableStyleLight9",
               colNames = TRUE, withFilter = TRUE,
               keepNA = TRUE, na.string = "NA")

saveWorkbook(wb, paste0(external_reports_dir, "/CAPTND_opti_summary_mth_", latest_date, ".xlsx"), overwrite = TRUE)

saveWorkbook(wb, paste0("../../../../optimised_data_report/CAPTND_opti_summary_mth_", latest_date, ".xlsx"), overwrite = TRUE)

}





