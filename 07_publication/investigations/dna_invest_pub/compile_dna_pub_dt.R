########################################################################.
### Update and save CAPTND DNA publication supplementary data tables ###
########################################################################. 

# Author: Luke Taylor
# Date: 2026-07-01

compile_dna_pub_data_tables <- function(dataset_choice = c("CAMHS", "PT")){
  
  assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
  
  wb <- loadWorkbook("../../../report_templates/publication/dna_pub_data_table_template_CAMHS.xlsx") |> 
    update_dna_dt_wording() |> 
    update_dna_dt_values() |> 
    protect_dna_worksheets() 
  
  saveWorkbook(wb, 
               paste0(shorewise_pub_report_dir, "/CAPTND_dna_data_tables_", 
                      dataset_choice, "_", publication_month, ".xlsx"), 
               overwrite = TRUE)
  
}



