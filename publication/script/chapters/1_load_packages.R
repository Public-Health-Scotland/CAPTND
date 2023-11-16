
#####################.
### Load packages ###
#####################.

# Author: Charlie Smith
# Date: 2023-11-14


if (!require("pacman")) install.packages("pacman")
pacman::p_load(openxlsx, 
               readxl, 
               tidyr,
               plyr, 
               dplyr, 
               lubridate, 
               stringr, 
               forcats, 
               gtools,
               ggplot2,
               glue,
               arrow, 
               rio,
               conflicted)


conflict_prefer('filter','dplyr')
conflict_prefer('mutate','dplyr')
