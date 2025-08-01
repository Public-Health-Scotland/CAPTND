
comp_reports_dir <- ("//PHI_conf/MentalHealth5/CAPTND/captnd_agg_comparison_report")

#hb <- 'NHS Ayrshire and Arran'

  compile_captnd_agg_comp_tables <- function(dataset_choice = c("CAMHS", "PT")){
    
    for (hb in hb_vector){
    
    source('06_calculations/captnd_agg_comparison_dt_values.R')
    source('06_calculations/captnd_agg_comparison_dt_wording.R')
    source('06_calculations/captnd_agg_comparison_protect_wb.R')
    
    assign(x = "hb", value = hb, envir = .GlobalEnv)
    assign(x = "dataset_choice", value = dataset_choice, envir = .GlobalEnv)
    
    wb <- loadWorkbook("../../../report_templates/data_quality/captnd_agg_comp_dt_template_hb.xlsx") |> 
      captnd_agg_comp_dt_wording() |> 
      captnd_agg_comp_dt_values() |> 
      captnd_agg_comp_protect_wb() 
    
    saveWorkbook(wb, 
                 paste0(comp_reports_dir, "/CAPTND_agg_comp_tables_", hb, "_", 
                        dataset_choice, "_", sub_month_end, ".xlsx"), 
                 overwrite = TRUE)
}

}