#this runs when the project opens to set up the file structure and 
#last analysis date (aka last time a swift file was opened and quality checked)

source("setup/data_analysis_latest_date.R")
source("setup/set_dir_structure.R")
source("setup/new_colnames.R")
source("ideas_space/find_a_captnd_file.R")

library(crayon)
library(beepr)
library(odbc)
library(rstudioapi)
library(dbplyr)
library(plyr)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(arrow)
library(forcats)
library(ggplot2)
library(plotly)
library(conflicted)
library(phsmethods)
library(stringr)
library(readr)
library(rio)
library(magrittr)
library(janitor)
library(phsstyles)
library(htmlwidgets)
library(parallelly)

conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')
conflict_prefer('summarise', 'dplyr')
conflict_prefer('rename','dplyr')
conflict_prefer('count', 'dplyr')
conflict_prefer('arrange','dplyr')

conflict_prefer('select','dplyr')
conflict_prefer('case_when','dplyr')
conflict_prefer('order_by','dplyr')
conflict_prefer('lag','dplyr')
conflict_prefer('lead','dplyr')
conflict_prefer('first','dplyr')
conflict_prefer('export','rio')

cat(bold(bgCyan(black(
"##########################################################
                    |    |    |                           
                   )_)  )_)  )_)                          
                  )___))___))___)\                         
                 )____)____)_____)\\                       
               _____|____|____|____\\__                    
      ---------\                   /---------              
        ^^^^^ ^^^^^^^^^^^^^^^^^^^^^                       
          ^^^^      ^^^^     ^^^    ^^                    
               ^^^^      ^^^                              
              Welcome to CAPTND, sailor!                  
 To find a file please use '///search_for_file()'. Ahoy!  
##########################################################\n"))))
