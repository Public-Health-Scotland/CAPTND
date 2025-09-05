
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

  dq_template_loc <- '../../../report_templates/data_quality/dq_report_template_DO_NOT_EDIT11.xlsx' 
  
  wb <- loadWorkbook(dq_template_loc) |> 
    update_dq_wording() |> 
    add_dq_heatmaps() |> 
    update_dq_values() 
  
  wb <- wb |> 
    # update_formulas() |> # commented out - formulas now take place in Excel Template now instead.
    protect_dq_worksheets() 
  

  # dq_template_loc <- '../../../report_templates/data_quality/archive/dq_report_template_DO_NOT_EDIT6.xlsx'
  # 
  # wb <- loadWorkbook(dq_template_loc) |>
  #   update_dq_wording() |>
  #   add_dq_heatmaps() |>
  #   update_dq_values()
  # 
  # wb <- wb |>
  #   update_formulas() |> # in progress
  #   protect_dq_worksheets()

  #path <- paste0(external_reports_dir, "/data_quality_report/dq_report_", month_latest,"_new.xlsx")
  path <- paste0(external_reports_dir, "/dq_report_", month_latest,"_new.xlsx")

  saveWorkbook(wb,
               path,
               overwrite = TRUE)

  # save to top level for "easy" access
  # saveWorkbook(wb,
  #              paste0("../../../../data_quality_report/dq_report_",  month_latest, "_new.xlsx"),
  #              overwrite = TRUE)

  message(paste0("Success! The report was saved to ", external_reports_dir))

}


