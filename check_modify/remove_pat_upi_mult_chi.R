################################################################################.
#### remove patients with upis that point to multiple patients and chi is NA ###
################################################################################.

library(dplyr)

filter_non_unique_upi <- function(df) {
  chis_per_upi <- df %>% 
    select(!!hb_name_o, !!dataset_type_o, !!upi_o, !!chi_o) %>% 
    distinct() %>% 
    filter(!is.na(!!sym(upi_o))) %>% 
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o),!!sym(upi_o)) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
  
  upi_and_chi  <- df %>% select(!!hb_name_o, !!dataset_type_o, !!upi_o, !!chi_o)
  
  chis_per_upi_with_chis_to_remove <- chis_per_upi %>% 
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o)) %>% 
    inner_join(upi_and_chi, by=c(upi_o, hb_name_o, dataset_type_o)) %>% 
    ungroup() %>% 
    distinct() %>% 
    filter(is.na(!!sym(chi_o)))
  
  df_filtered <- df %>% 
    anti_join(chis_per_upi_with_chis_to_remove, by=c(hb_name_o, dataset_type_o, upi_o, chi_o))
  
  
  chis_per_upi_with_chis_to_remove_stats <- chis_per_upi_with_chis_to_remove %>% 
    inner_join(df, by=c(hb_name_o, dataset_type_o, upi_o, chi_o)) %>% 
    group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
    summarise(removed_records=n()) %>% 
    ungroup()
  
  write_csv(chis_per_upi_with_chis_to_remove_stats, 
            paste0("../../../output/removed/multi_chis_per_upi_",today(),".csv"))
  
  message(paste("Stats on removed files due to UPI not being unique to patient, and CHI being NA are saved in\n",
                paste0("../../../output/removed/multi_chis_per_upi_",today(),".csv")))
  
  return(df_filtered)
}

