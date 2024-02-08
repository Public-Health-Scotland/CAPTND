################################################################################.
#### remove patients with upis that point to multiple patients and chi is NA ###
################################################################################.

source('05_data_quality/report_removed_upi_mult_chi.R')
# library(dplyr)

filter_non_unique_upi <- function(df,stage_name) {
  chis_per_upi <- df %>% 
    select(!!hb_name_o, !!dataset_type_o, !!upi_o, !!chi_o) %>% 
    distinct() %>% 
    filter(!is.na(!!sym(upi_o))) %>% 
    group_by(!!sym(hb_name_o), !!sym(dataset_type_o),!!sym(upi_o)) %>% 
    summarise(n=n(), .groups = 'drop') %>% 
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
  
  
  chis_per_upi_with_chis_to_remove_with_dates=df %>% 
    select(!!hb_name_o, !!dataset_type_o, !!upi_o, !!chi_o, !!header_date_o) %>% 
    distinct() %>% 
    inner_join(chis_per_upi_with_chis_to_remove, by=c(hb_name_o, dataset_type_o, upi_o, chi_o))
  
  write_csv(chis_per_upi_with_chis_to_remove_with_dates %>% 
              mutate(issue='non unique UPI', 
              !!patient_id_o:=!!sym(upi_o)) %>%
              select(-c(n, !!upi_o,!!chi_o)),
            paste0(removed_data_export_dir,
                   '/',
                   stage_name,
                   '_removed_non_unique_upi_details',
                   '.csv'))
  
  report_upi_mult_chi(df,chis_per_upi_with_chis_to_remove, stage_name)
 
  
  return(df_filtered)
}

