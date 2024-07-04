
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
  
  # by dataset, hb, simd - maybe this is a better place to start for all pops???
  df_datazone_pop <- readRDS('/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2021.rds') |> 
    filter(year == max(year)) |> # latest year is 2021 population
    select(hb2019name, simd2020v2_sc_quintile, sex, 5:95) |> 
    pivot_longer(cols = 4:94, names_to = 'age', values_to = 'count') |> 
    mutate(sex = if_else(sex == "M", 1, 2),
           age = str_remove(age, c("age")),
           age = as.numeric(str_remove(age, c("plus"))),
           dataset_type = "PT") |> 
    rename(hb_name = hb2019name, 
           simd2020_quintile = simd2020v2_sc_quintile)
    
  df_all <- df_datazone_pop |> 
    filter(age <= 17) |> 
    mutate(dataset_type = "CAMHS") |> 
    rbind(df_datazone_pop) 
  
  # hb pop by dataset
  df_ds_hb_pop <- df_all |> 
    group_by(dataset_type, hb_name) |> 
    summarise(population = sum(count)) |> 
    group_by(dataset_type) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop"))  |> 
    arrange(dataset_type, hb_name)
    
  # hb by sex pop
  df_ds_hb_sex_pop <- df_all |> 
    group_by(dataset_type, hb_name, sex) |> 
    summarise(population = sum(count)) |> 
    group_by(dataset_type, sex) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop"))  |> 
    arrange(dataset_type, hb_name)
  
  # hb by age group pop
  # CONTINUE....
  
  
  # hb by simd pop
  df_ds_hb_simd_pop <- df_all |> 
    group_by(dataset_type, hb_name, simd2020_quintile) |> 
    summarise(population = sum(count)) |> 
    group_by(dataset_type, simd2020_quintile) %>% 
    bind_rows(summarise(.,
                        across(where(is.numeric), sum),
                        across(hb_name, ~"NHS Scotland"),
                        .groups = "drop"))  |> 
    arrange(dataset_type, hb_name)
  
}


