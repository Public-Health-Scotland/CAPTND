
##############################.
### Compile Excel Document ###
##############################.

# Author: Charlie Smith
# Date: 2023-11-20


compile_excel_doc <- function(){
  
  # load data
  table_refs_camhs <- read_parquet(paste0(data_working_safe, 'table_refs_quarterly_CAMHS.parquet'))
  table_refs_pt <- read_parquet(paste0(data_working_safe, 'table_refs_quarterly_PT.parquet'))
  
  # save in doc
  tabs <- list(refs_camhs = table_refs_camhs, 
               refs_pt = table_refs_pt)
  
  export(tabs, paste0(data_working_safe, 'data_for_publication.xlsx'))
  
}
