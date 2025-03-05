##############################################.
### Update and save CAPTND MMI data tables ###
##############################################. 

# Author: Luke Taylor
# Date: 2024-09-30

compile_mmi_data_tables <- function(dataset_choice = c("CAMHS", "PT")){
  
  assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
  
  wb <- loadWorkbook("../../../report_templates/mmi/mmi_data_table_template_CAMHS.xlsx") |> 
    update_mmi_dt_wording(time_period = 'Monthly') |> 
    update_mmi_dt_values(time_period = 'Monthly') |> 
    protect_mmi_worksheets(time_period = 'Monthly') 
  
  saveWorkbook(wb, 
               paste0("/PHI_conf/MentalHealth5/CAPTND/mmi_report/", "mmi_data_tables_",
                      dataset_choice, "_", publication_month, ".xlsx"), 
               overwrite = TRUE)
  
}



