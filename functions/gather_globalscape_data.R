
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
  captnd_all <- con <- tryCatch({
    load_globalscape_data()
  }, warning = function(w) {
    message('there was an issue with your name/password and connection to db could not be established')
  }, error = function(e) {
    message('there was an error with your name/password and connection to db could not be established')
  })
    
  
  # 3 - Join data -----------------------------------------------------------
  
  # 3.1 - Stack stages ------------------------------------------------------
  captnd_all <- stack_stages(captnd_all)
  
  # 3.2 - Stack new and return appointments only ----------------------------
  captnd_all <- stack_new_return_apps(captnd_all)
  
  # 4 - Set column names ----------------------------------------------------
  captnd_all <- globalscape_column_renamer(captnd_all)

  return(captnd_all)
  
}



