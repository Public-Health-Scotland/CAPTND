

##############################.
### Load Data Quality Data ###
##############################.

# Author: Charlie Smith
# Date: 2024-05-03

load_dq_data <- function(){
  
  df <- read_parquet(paste0(data_prep_dir, "/captnd_checked.parquet")) |> 
    mutate(header_date_month = floor_date(!!sym(header_date_o), unit = "month"), .after = !!sym(header_date_o))
  
  return(df)
  
}


