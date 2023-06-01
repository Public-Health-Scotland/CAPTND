
##################################################.
### CAPTND: Load and join old Globalscape data ###
##################################################.

# NB this script only works for charlie and maria (joana does not have access 
# to all staging areas)

# 1 - Housekeeping --------------------------------------------------------

# 1.1 - Load packages -----------------------------------------------------

library(odbc)
library(rstudioapi)
library(plyr)
library(dbplyr)
library(dplyr)
library(purrr)


# 1.2 - Source functions --------------------------------------------------

source("./functions/globalscape_data_loader.R")
source("./functions/globalscape_stack_stages.R")
source("./functions/globalscape_stack_new_return_apps.R")
source("./functions/globalscape_column_renamer.R")


gather_globalscape <- function() {
  
  # 2 - Load data -----------------------------------------------------------
  captnd_all <- load_globalscape_data()
  
  # 3 - Join data -----------------------------------------------------------
  
  # 3.1 - Stack stages ------------------------------------------------------
  captnd_all <- stack_stages(captnd_all)
  
  # 3.2 - Stack new and return appointments only ----------------------------
  captnd_all <- stack_new_return_apps(captnd_all)
  
  # 4 - Set column names ----------------------------------------------------
  captnd_all <- globalscape_column_renamer(captnd_all)

  return(captnd_all)
  
}



# 5 - Save each list element as parquet -----------------------------------

out_dir <- "../../../output/" 
outfile <- paste0("extract_", Sys.Date())

filepath <- paste0(out_dir, outfile)
dir.create(filepath)

list_names <- names(df_glob_raw)

for(i in seq_along(list_names)){
  
  df_list <- df_glob_raw[i] %>% 
    data.frame()
  
  save_as_parquet(df = df_list, path = paste0(filepath, "/glob_", list_names[i]))
 
}


