#########################################################.
### Generate extract files for specific health boards ###
#########################################################.

# Author: Luke Taylor
# Date: 2025-06-16

# 1 Set constants---------------------------------------------------------------
month_end <- "2025-05-01"

source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

# 2 Source extract scripts

source("./06_calculations/write_wl_extract_csv.R")
source("./06_calculations/write_pats_seen_extract_csv.R")
source("./06_calculations/write_missing_refs_extract_csv.R")

# 3 Run functions

write_wl_extract('NHS ', 'CAMHS')
write_pats_seen_extract('NHS Highland', 'PT')
write_missing_ref_extract('NHS Highland', 'PT')


