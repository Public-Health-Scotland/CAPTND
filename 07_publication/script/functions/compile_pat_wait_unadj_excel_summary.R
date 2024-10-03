
###########################################.
### Compile patient waits excel summary ###
###########################################.

# Author: Luke Taylor
# Date: 2024-07-23


compile_pat_wait_unadj_summary <- function(){
  
  # get file names
  #filnames <- list.files(ref_dir)
  
  #  load parquet files 
  p1 <- read_parquet(paste0(pat_waits_dir, "patients_wait_month_hb.parquet"))
  p2 <- read_parquet(paste0(pat_waits_dir, "patients_wait_month_hb_age.parquet"))
  p3 <- read_parquet(paste0(pat_waits_dir, "patients_wait_month_hb_sex.parquet"))
  p4 <- read_parquet(paste0(pat_waits_dir, "patients_wait_month_hb_simd.parquet"))
  
  p5 <- read_parquet(paste0(pat_waits_dir, "patients_wait_quarter_hb.parquet"))
  p6 <- read_parquet(paste0(pat_waits_dir, "patients_wait_quarter_hb_age.parquet"))
  p7 <- read_parquet(paste0(pat_waits_dir, "patients_wait_quarter_hb_sex.parquet"))
  p8 <- read_parquet(paste0(pat_waits_dir, "patients_wait_quarter_hb_simd.parquet"))
  
  # name tabs
  list_tabs <- list(
    month_hb = p1,
    month_sex = p2,
    month_age = p3,
    month_simd = p4,
    
    quart_hb = p5,
    quart_sex = p6,
    quart_age = p7,
    quart_simd = p8)
  
  # save output as excel doc
  filepath = paste0(shorewise_pub_measure_summaries_dir, "/patients_waiting_unadjusted_summary.xlsx")
  export(list_tabs, file = filepath)
  
  # format report
  wb <- loadWorkbook(filepath)
  
  for(i in 1:length(list_tabs)){
    setColWidths(wb, sheet = i, cols = 1:9, widths = "auto")
  }
  
  saveWorkbook(wb, filepath, overwrite =TRUE)
  
}



