
###########################################.
### Compile basic vs opti excel summary ###
###########################################.

# Author: Bex Madden
# Date: 2024-07-23


compile_basic_opti_summary <- function(){
  
  # get file names
  #filnames <- list.files(ref_dir)
  
  #  load parquet files 
  p1 <- read_parquet(paste0(basic_opti_dir, "refs_basic_opti_monthly.parquet"))
  p2 <- read_parquet(paste0(basic_opti_dir, "refs_basic_opti_quarterly.parquet"))

  
  # name tabs
  list_tabs <- list(
    by_month = p1,
    by_quarter = p2)
  
  # save output as excel doc
  filepath = paste0(shorewise_pub_measure_summaries_dir, "/basic_opti_summary.xlsx")
  export(list_tabs, file = filepath)
  
  # format report
  wb <- loadWorkbook(filepath)
  
  for(i in 1:length(list_tabs)){
    setColWidths(wb, sheet = i, cols = 1:9, widths = "auto")
  }
  
  saveWorkbook(wb, filepath, overwrite =TRUE)
  
}



