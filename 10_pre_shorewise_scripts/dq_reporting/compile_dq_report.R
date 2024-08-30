
#########################.
### Compile DQ report ###
#########################.

# Author: Charlie Smith
# Date: 2024-08-30


# Plan:

# load template workbook
# update wording where necessary
# update data tables
# protect worksheets
# save separate output


compile_dq_report <- function(){
  
  #assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
  
  wb <- loadWorkbook("../../../output/pub_templates_KEEP/dq_report_template_DO_NOT_EDIT.xlsx") |> 
    update_dq_wording() |> 
    add_dq_heatmaps() |> 
    #update_dq_values() |> # continue...
    #protect_dq_worksheets() 
  
  saveWorkbook(wb, 
               # paste0(shorewise_pub_report_dir, "/CAPTND_data_tables_", 
               #        dataset_choice, "_", publication_month, ".xlsx"), 
               overwrite = TRUE)
  
}


