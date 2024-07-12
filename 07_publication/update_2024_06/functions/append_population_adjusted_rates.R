
########################################.
### Append population adjusted rates ###
########################################.

# Author: Charlie Smith
# Date: 2024-06-07


append_pop_adjusted_rates <- function(df, pop_type = c("all", "sex", "agegp", "simd")){
  
  # load required reference pop
  
  if(pop_type == "all"){
    
    df_pop <- read_rds('../../../output/publication/data/reference_files/ds_hb_pop.rds') |> 
      left_join(df, by = c("dataset_type", "hb_name")) |> 
      mutate(pop_rate_1000 = count / population * 1000)
    
  } else {
    
    if(pop_type == "sex"){
      
      df_pop <- read_rds('../../../output/publication/data/reference_files/ds_hb_sex_pop.rds')
      
    } else {
      
      if(pop_type == "agegp"){
        
        df_pop <- read_rds('../../../output/publication/data/reference_files/ds_hb_agegp_pop.rds')
      
      } else {
      
        if(pop_type == "simd"){
          
          df_pop <- read_rds('../../../output/publication/data/reference_files/ds_hb_simd_pop.rds')
          
        } # simd
        
      } # agegp
        
    } # sex
    
  } # all
  
  # return(df_pop)
  
}    

test <- append_pop_adjusted_rates(pop_type = "all")

