
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
  
  dq_template_loc <- '../../../report_templates/data_quality/dq_report_template_DO_NOT_EDIT4.xlsx' 
  
  wb <- loadWorkbook(dq_template_loc) |> 
    update_dq_wording() |> 
    add_dq_heatmaps() |> 
    update_dq_values() |> 
    protect_dq_worksheets() 
  
  path <- paste0(external_reports_dir, "/dq_report_", month_latest,"_new.xlsx")
  
  saveWorkbook(wb, 
               path, 
               overwrite = TRUE)
  
  message(paste0("Success! The report was saved to ", external_reports_dir))
  
}


