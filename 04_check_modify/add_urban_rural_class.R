######################################
### Add Urban Rural Classification ###
######################################

add_urban_rural_class <- function(df){
  
  postcode_dir <- ("/conf/linkage/output/lookups/Unicode/Geography")
  
  postcode_lookup <- read_parquet(paste0(postcode_dir,
                                         '/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.parquet')) %>%
    select(pc8, ur8_2022_name)
  
  df_completed <- df %>%
    mutate(!!postcode_last_reported_o := format_postcode(!!sym(postcode_last_reported_o), format = 'pc8'), #add space before last 3 characters
           !!sym(postcode_last_reported_o) := toupper(!!sym(postcode_last_reported_o))) %>% # make postcodes all caps
    left_join(postcode_lookup, by = join_by(!!sym(postcode_last_reported_o) == pc8), multiple = "all")
    
  message('Urban Rural Class completed\n')
  
  return(df_completed)
}