
############################################.
### Compile open cases excel summary ###
############################################.

# Author: Charlie Smith
# Date: 2024-09-20


compile_open_cases_summary <- function(){
  
  # get file names
  #filnames <- list.files(open_dir)
  
  #  load parquet files 
  p1 <- read_parquet(paste0(open_dir, "open_cases_all_hb.parquet"))
  p2 <- read_parquet(paste0(open_dir, "open_cases_all_hb_sex.parquet"))
  p3 <- read_parquet(paste0(open_dir, "open_cases_all_hb_age.parquet"))
  p4 <- read_parquet(paste0(open_dir, "open_cases_all_hb_simd.parquet"))
  
  p5 <- read_parquet(paste0(open_dir, "open_cases_month_hb.parquet"))
  p6 <- read_parquet(paste0(open_dir, "open_cases_month_hb_sex.parquet"))
  p7 <- read_parquet(paste0(open_dir, "open_cases_month_hb_age.parquet"))
  p8 <- read_parquet(paste0(open_dir, "open_cases_month_hb_simd.parquet"))
  
  p9 <- read_parquet(paste0(open_dir, "open_cases_quarter_hb.parquet"))
  p10 <- read_parquet(paste0(open_dir, "open_cases_quarter_hb_sex.parquet"))
  p11 <- read_parquet(paste0(open_dir, "open_cases_quarter_hb_age.parquet"))
  p12 <- read_parquet(paste0(open_dir, "open_cases_quarter_hb_simd.parquet"))
  
  # name tabs
  list_tabs <- list(
    all_hb = p1,
    all_sex = p2,
    all_age = p3,
    all_simd = p4,
    
    month_hb = p5,
    month_sex = p6,
    month_age = p7,
    month_simd = p8,
    
    quart_hb = p9,
    quart_sex = p10,
    quart_age = p11,
    quart_simd = p12)
  
  # save output as excel doc
  filepath = paste0(shorewise_pub_measure_summaries_dir, "/open_cases_summary.xlsx")
  export(list_tabs, file = filepath)
  
  # format report
  wb <- loadWorkbook(filepath)
  
  for(i in 1:length(list_tabs)){
    setColWidths(wb, sheet = i, cols = 1:9, widths = "auto")
  }
  
  saveWorkbook(wb, filepath, overwrite =TRUE)
  
}
