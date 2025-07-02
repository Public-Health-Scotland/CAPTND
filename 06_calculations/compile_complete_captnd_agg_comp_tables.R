
comp_reports_dir <- ("//PHI_conf/MentalHealth5/CAPTND/captnd_agg_comparison_report")


compile_complete_captnd_agg_comp_tables <- function(dataset_choice = c("CAMHS", "PT")){
    
    source('06_calculations/captnd_agg_comparison_dt_values_comp.R')
    source('06_calculations/captnd_agg_comparison_dt_wording.R')
    source('06_calculations/captnd_agg_comparison_protect_wb.R')
    
    assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
    
    wb <- loadWorkbook("../../../report_templates/data_quality/captnd_agg_comp_dt_template.xlsx") |> 
      captnd_agg_comp_dt_wording() |> 
      captnd_agg_comp_dt_values_comp() |> 
      captnd_agg_comp_protect_wb() 
    
    saveWorkbook(wb, 
                 paste0(comp_reports_dir, "/CAPTND_agg_comp_tables_", "_", 
                        dataset_choice, "_", sub_month_end, ".xlsx"), 
                 overwrite = TRUE)
  
}

