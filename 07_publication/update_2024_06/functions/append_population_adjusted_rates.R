
########################################.
### Append population adjusted rates ###
########################################.

# Author: Charlie Smith
# Date: 2024-06-07


append_population_adjusted_rates <- function(df){
  
  # //Stats/cl-out/lookups/Unicode/Geography/
  df_hb_age_sex <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_5year_agegroups_1981_2022.rds') |> 
    rename(hb_name = hb2019name) |> 
    filter(year == max(year)) 
  
  df_pt <- df_hb_age_sex |> 
    mutate(dataset_type = "PT", .before = everything())
    
  df_camhs <- df_hb_age_sex |> 
    filter(age_group %in% c(0:5)) |> 
    mutate(dataset_type = "CAMHS", .before = everything())
    
  
  # basic dataset and HB pops
  df_pt_pop <- df_pt |> 
    group_by(dataset_type, hb_name) |> 
    summarise(pop = sum(pop), .groups = "drop")
  
  df_camhs_pop <- df_camhs |> 
    group_by(dataset_type, hb_name) |> 
    summarise(pop = sum(pop), .groups = "drop")
  
  df_ds_hb_pop <- rbind(df_pt_pop, df_camhs_pop) |> 
    group_by(dataset_type) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    arrange(dataset_type)
    
  rm(df_pt_pop, df_camhs_pop)
  
  # by dataset, hb, sex
  df_pt_pop <- df_pt |> 
    group_by(dataset_type, hb_name, sex) |> 
    summarise(pop = sum(pop), .groups = "drop")
  
  df_camhs_pop <- df_camhs |> 
    group_by(dataset_type, hb_name, sex) |> 
    summarise(pop = sum(pop), .groups = "drop")
  
  df_ds_hb_sex_pop <- rbind(df_pt_pop, df_camhs_pop) |> 
    group_by(dataset_type, sex) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop")) |> 
    arrange(dataset_type, sex)
  
  
  # by dataset, hb, age
  # CONTINUE...
  
  # by dataset, hb, simd
  
  
  #df_hb_simd <- read_parquet('/conf/linkage/output/lookups/Unicode/Deprivation/postcode_2024_1_simd2020v2.parquet') 
  
  

  
  
}


