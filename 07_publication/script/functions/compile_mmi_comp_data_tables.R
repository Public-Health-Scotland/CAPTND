##############################################.
### Update and save CAPTND MMI data tables ###
##############################################. 

# Author: Luke Taylor
# Date: 2024-09-30

compile_complete_mmi_data_tables <- function(dataset_choice = c("CAMHS", "PT")){
  
  source('07_publication/script/functions/mmi_update_dt_values_comp.R')
  #source('07_publication/script/functions/mmi_protect_worksheets.R')
  source('07_publication/script/functions/mmi_update_dt_wording_comp.R')
  
  assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
  
  wb <- loadWorkbook("../../../report_templates/mmi/mmi_data_table_template_CAMHS_sg.xlsx") |> 
    update_mmi_dt_wording_comp(time_period = 'Monthly') |> 
    update_mmi_dt_values_comp(time_period = 'Monthly') #|> 
    #protect_mmi_worksheets(time_period = 'Monthly') 
  
  saveWorkbook(wb, 
               paste0("/PHI_conf/MentalHealth5/CAPTND/mmi_report/", "mmi_data_tables_",
                      dataset_choice, "_", month_end, ".xlsx"), 
               overwrite = TRUE)
  
}



