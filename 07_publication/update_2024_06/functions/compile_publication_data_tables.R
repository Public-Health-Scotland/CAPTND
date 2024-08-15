
####################################################################.
### Update and save CAPTND publication supplementary data tables ###
####################################################################. 

# Author: Charlie Smith
# Date: 2024-08-08

# Plan:
# for CAMHS and PT... (loop?)
# load template workbook
# update wording where necessary
# update data tables
# add alt text?
# protect worksheets
# save separate output


compile_pub_data_tables <- function(dataset_choice = c("CAMHS", "PT")){
  
  wb <- loadWorkbook("../../../output/pub_templates_KEEP/data_table_template_CAMHS.xlsx") |> 
    update_dt_wording() |> 
    update_dt_values() |> # add alt text!
    protect_worksheets() 
    
    saveWorkbook(wb, 
                 paste0(shorewise_pub_report_dir, "/CAPTND_data_tables_", 
                        dataset_choice, "_", publication_month, ".xlsx"), 
                 overwrite = TRUE)
    
}
