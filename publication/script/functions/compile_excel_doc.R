
##############################.
### Compile Excel Document ###
##############################.

# Author: Charlie Smith
# Date: 2023-11-20


compile_excel_doc <- function(){
  
  # load CAMHS data
  table_refs_camhs <- read_parquet(paste0(data_working_safe, 'table_refs_quarterly_CAMHS.parquet'))
  
  refs_monthly_camhs <- read_parquet(paste0(data_working_safe, 'refs_monthly_sco.parquet')) |> 
    filter(!!sym(dataset_type_o) == "CAMHS")
  
  refs_sex_age_camhs <- read_parquet(paste0(data_working_safe, 'refs_sex_age.parquet')) |> 
    filter(!!sym(dataset_type_o) == "CAMHS")
  
  ref_simd_camhs <- read_parquet(paste0(data_working_safe, 'refs_simd.parquet')) |> 
    filter(!!sym(dataset_type_o) == "CAMHS" &
          !!sym(simd_quintile_o) %in% c(1, 5)) |> 
    arrange(!!sym(simd_quintile_o)) |> 
    mutate(!!referral_month_o := as.character(!!sym(referral_month_o)))
  
  
  # save CAMHS doc
  tabs <- list(refs_camhs = table_refs_camhs, 
               refs_monthly = refs_monthly_camhs, 
               refs_sex_age = refs_sex_age_camhs, 
               ref_simd = ref_simd_camhs)
  
  rio::export(tabs, paste0(data_working_safe, 'data_for_pub_CAMHS.xlsx'))
  
  
  # load PT data
  table_refs_pt <- read_parquet(paste0(data_working_safe, 'table_refs_quarterly_PT.parquet'))
  
  refs_monthly_pt <- read_parquet(paste0(data_working_safe, 'refs_monthly_sco.parquet')) |> 
    filter(!!sym(dataset_type_o) == "PT")
  
  refs_sex_age_pt <- read_parquet(paste0(data_working_safe, 'refs_sex_age.parquet')) |> 
    filter(!!sym(dataset_type_o) == "PT")
  
  ref_simd_pt <- read_parquet(paste0(data_working_safe, 'refs_simd.parquet')) |> 
    filter(!!sym(dataset_type_o) == "PT" &
             !!sym(simd_quintile_o) %in% c(1, 5)) |> 
    arrange(!!sym(simd_quintile_o)) |> 
    mutate(!!referral_month_o := as.character(!!sym(referral_month_o)))
  
  # save CAMHS doc
  tabs <- list(refs_pt = table_refs_pt, 
               refs_monthly = refs_monthly_pt, 
               refs_sex_age = refs_sex_age_pt, 
               ref_simd = ref_simd_pt)
  
  rio::export(tabs, paste0(data_working_safe, 'data_for_pub_PT.xlsx'))
  
}
