

extract_chi_upi_pat_id <- function(df) {
  
  df_upi_chi_pat_id <- df %>% 
    select(!!upi_o,!!patient_id_o,!!chi_o) 
  
  return(df_upi_chi_pat_id)

}