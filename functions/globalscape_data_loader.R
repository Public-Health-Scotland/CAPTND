
###############################.
### Globalscape Data Loader ### 
###############################.

# 1 - Load packages  -----------------------------------------------------------

library(conflicted)
library(stringr)
library(dplyr)
library(odbc)
library(rstudioapi)
conflicts_prefer(dplyr::rename)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::mutate)

# 2 - Function to load globalscape data  ---------------------------------------

load_globalscape_data <- function(con){
  
  con <- dbConnect(odbc(),
                   dsn = "CAPTND",
                   uid = askForPassword("Enter network username:"), 
                   pwd = askForPassword("Enter network password:"))
  
  # set vector of staging areas to loop through
  staging_areas <- c("CAMHS_REFERRAL_STAGE", "CAMHS_NEW_STAGE", "CAMHS_RETURN_STAGE",
                     "CAMHS_DIAGNOSIS_STAGE", "CAMHS_OUTCOMES_STAGE", "CAMHS_DISCHARGE_STAGE",
                     "PT_REFERRAL_STAGE", "PT_NEW_STAGE", "PT_RETURN_STAGE",
                     "PT_DIAGNOSIS_STAGE", "PT_OUTCOMES_STAGE", "PT_DISCHARGE_STAGE")
  
  list_bucket <- list() # create empty list to store data
  
  for(i in 1:length(staging_areas)){ # loop through each element of staging areas
    
    ob_dataset_type <- if_else(substr(staging_areas[i], 1, 1) == "C", "CAMHS", "PT") # efficient way of getting names? Better to use list names?
    ob_record_type <- tolower(if_else(substr(staging_areas[i], 1, 1) == "C", 
                                      substr(staging_areas[i], 7, nchar(staging_areas[i])-6),
                                      substr(staging_areas[i], 4, nchar(staging_areas[i])-6)))
    
    df_area <- as.data.frame(tbl(con, in_schema("CAPTND", staging_areas[i]))) %>% # load each staging area
      dplyr::mutate(dataset_type = ob_dataset_type,
             record_type = ob_record_type,
             sub_source = "globalscape")
    
    list_bucket[[i]] <- df_area # add each staging area to empty list
    
  }
  
  names(list_bucket) <- tolower(substr(staging_areas, # set names in list using staging_areas vector
                                       start = 1, 
                                       stop = nchar(staging_areas)-6)) # remove "_STAGE" from names
  
  return(list_bucket) 
  
}

