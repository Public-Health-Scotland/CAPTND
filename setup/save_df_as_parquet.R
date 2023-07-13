
###########################################.
### Function to save df as parquet file ###
###########################################.


# 1 - Housekeeping --------------------------------------------------------

# Load required packages
library(tidyverse)
library(arrow)
library(parallelly)



# 2 - Function ------------------------------------------------------------

save_as_parquet <- function(df, path){
  
  # get number of CPUs available to the session
  n_cpus <- as.numeric(parallelly::availableCores())
  
  # tell {arrow} how many CPUs it can use
  arrow::set_cpu_count(n_cpus)
  
  # save df
  arrow::write_parquet(df, # df to save
                       sink = paste0(path, ".parquet"), # path to save directory (.parquet extension added automatically)
                       compression = "zstd")
  
}

