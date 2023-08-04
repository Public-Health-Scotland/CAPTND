################################################################################.
#### remove patients with upis that point to multiple patients and chi is NA ###
################################################################################.

library(dplyr)

filter_non_unique_upi <- function(df) {
  chis_per_upi <- df %>% 
    select(hb_name, dataset_type,upi,chi) %>% 
    distinct() %>% 
    filter(!is.na(upi)) %>% 
    group_by(hb_name, dataset_type,upi) %>% 
    summarise(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
  
  upi_and_chi=df %>% select(hb_name, dataset_type,upi,chi)
  
  chis_per_upi_with_chis_to_remove = chis_per_upi %>% 
    group_by(hb_name,dataset_type) %>% 
    inner_join(upi_and_chi, by=c('upi','hb_name', 'dataset_type'))%>% 
    ungroup() %>% 
    distinct() %>% 
    filter(is.na(chi))
  
  df_filtered <- df %>% 
    anti_join(chis_per_upi_with_chis_to_remove, by=c('hb_name', 'dataset_type','upi','chi'))
  
  
  chis_per_upi_with_chis_to_remove_stats <- chis_per_upi_with_chis_to_remove %>% 
    inner_join(df, by=c('hb_name', 'dataset_type','upi','chi')) %>% 
    group_by(hb_name,dataset_type) %>% 
    summarise(removed_records=n()) %>% 
    ungroup()
  
  return(df_filtered)
}

