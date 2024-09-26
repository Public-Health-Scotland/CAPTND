
##########################################.
### Compile appointments excel summary ###
##########################################.

# Author: Bex Madden
# Date: 2024-07-23


compile_pat_seen_unadj_summary <- function(){
  
  # get file names
  #filnames <- list.files(ref_dir)
  
  #  load parquet files 
  p1 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_all.parquet"))
  p2 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_all_sex.parquet"))
  p3 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_all_age.parquet"))
  p4 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_all_simd.parquet"))
  
  p5 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_mth.parquet"))
  p6 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_mth_sex.parquet"))
  p7 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_mth_age.parquet"))
  p8 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_mth_simd.parquet"))
  
  p9 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_qt.parquet"))
  p10 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_qt_sex.parquet"))
  p11 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_qt_age.parquet"))
  p12 <- read_parquet(paste0(pat_seen_dir, "pat_seen_unadj_wait_grp_qt_simd.parquet"))
  
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
  filepath = paste0(shorewise_pub_measure_summaries_dir, "/patients_seen_unadjusted_summary.xlsx")
  export(list_tabs, file = filepath)
  
  # format report
  wb <- loadWorkbook(filepath)
  
  for(i in 1:length(list_tabs)){
    setColWidths(wb, sheet = i, cols = 1:9, widths = "auto")
  }
  
  saveWorkbook(wb, filepath, overwrite =TRUE)
  
}



