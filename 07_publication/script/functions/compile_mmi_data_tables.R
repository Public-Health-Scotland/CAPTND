##############################################.
### Update and save CAPTND MMI data tables ###
##############################################. 

# Author: Luke Taylor
# Date: 2024-09-30

compile_mmi_data_tables <- function(dataset_choice = c("CAMHS", "PT")){
  
  assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
  
  wb <- loadWorkbook("../../../output/pub_templates_KEEP/mmi_data_table_template_CAMHS.xlsx") |> 
    update_mmi_dt_wording(time_period = 'Monthly') |> 
    update_mmi_dt_values(time_period = 'Monthly') |> 
    protect_mmi_worksheets(time_period = 'Monthly') 
  
  saveWorkbook(wb, 
               paste0(shorewise_pub_report_dir, "/mmi_data_tables_", 
                      dataset_choice, "_", publication_month, ".xlsx"), 
               overwrite = TRUE)
  
}



