
comp_reports_dir <- ("//PHI_conf/MentalHealth5/CAPTND/captnd_agg_comparison_report")

compile_captnd_agg_comp_tables <- function(dataset_choice = c("CAMHS", "PT")){
  
  assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
  
  wb <- loadWorkbook("../../../report_templates/data_quality/captnd_agg_comp_dt_template.xlsx") |> 
    captnd_agg_comp_dt_wording() |> 
    captnd_agg_comp_dt_values() |> 
    captnd_agg_comp_protect_wb() 
  
  saveWorkbook(wb, 
               paste0(comp_reports_dir, "/CAPTND_agg_comp_tables_", 
                      dataset_choice, "_", publication_month, ".xlsx"), 
               overwrite = TRUE)
  
}
