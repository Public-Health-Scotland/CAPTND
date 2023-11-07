
###############################################################.
### Append local authority name to CAPTND based on postcode ###
###############################################################.

# Author: Charlie Smith
# Date: 2023-11-07


df_test <- open_last_parquet_with_rrt_eval()


append_local_authority_res <- function(df){
  
  # load local authority lookup
  df_la <- read_csv_arrow("../../../data/small_area_postcode_index_23_1.csv") |> 
    select(Postcode, CouncilArea2019Code) |> 
    rename(CA = CouncilArea2019Code)
  
  # load la name lookup
  df_la_name <- read_csv_arrow(url("https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/967937c4-8d67-4f39-974f-fd58c4acfda5/download/ca11_ca19.csv")) |> 
    select(CA, CAName)
  
  # join to one lookup
  df_la_codes_names <- left_join(df_la, df_la_name, by = "CA", relationship = "many-to-many") |> 
    select(-CA) |> 
    rename(postcode = Postcode, !!local_authority_name_o := CAName) |> 
    mutate(postcode = str_replace(postcode, " ", ""), # remove spaces from postcodes
           postcode = toupper(postcode)) # make postcodes all caps
  
  
  # append LA names to CAPTND
  df_captnd <- left_join(df, df_la_codes_names, by = 
                         join_by(!!sym(postcode_last_reported_o) == postcode), 
                         relationship = "many-to-many")
  
  return(df_captnd)
  
}

test2 <- append_local_authority_res(df_test)
