######################################
### Add Urban Rural Classification ###
######################################

add_urban_rural_class <- function(df){
  
  # postcode_dir <- ("/conf/linkage/output/lookups/Unicode/Geography")
  
  spdfiles <- list.files("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/", pattern = "^Scottish_Postcode_Directory_.*\\.rds$", full.names = TRUE)
  
  # Get file info and identify the most recent file
  spdlatest_file <- spdfiles %>%
    file.info() %>%
    arrange(desc(mtime)) %>%
    rownames() %>%
    .[1]
  
  # Import the most recent version of the postcode file
  postcode_load <- readRDS(spdlatest_file)  |> 
    select(pc8, ur8_2022_name,ca2019,ca2019name,hscp2019, hscp2019name, datazone2022,datazone2022name,datazone2011,intzone2022, intzone2022name)
  
  hscp_locality <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.rds') |>
    select(datazone2011, hscp_locality)
  
  postcode_lookup <- postcode_load %>%
    left_join(hscp_locality, by = "datazone2011") |>
    select(pc8, ur8_2022_name,ca2019,ca2019name,hscp_locality,hscp2019, hscp2019name, datazone2022,datazone2022name,intzone2022, intzone2022name)
  
  # postcode_lookup2 <- read_parquet(paste0(postcode_dir,
  #                                        '/Scottish Postcode Directory/Scottish_Postcode_Directory_2025_1.parquet')) %>%
  #   select(pc8, ur8_2022_name)
  
  df_completed <- df %>%
    mutate(!!postcode_last_reported_o := format_postcode(!!sym(postcode_last_reported_o), format = 'pc8'), #add space before last 3 characters
           !!sym(postcode_last_reported_o) := toupper(!!sym(postcode_last_reported_o))) %>% # make postcodes all caps
    left_join(postcode_lookup, by = join_by(!!sym(postcode_last_reported_o) == pc8), multiple = "all")
  
  message('Urban Rural Class completed\n')
  
  return(df_completed)
}