##########################.
###  Make patient id   ###
##########################.


#Fill patient_id with UPI in case CHI is NA
add_patient_id <- function(df){
  
  df_with_patient_id <- df %>% 
    mutate(!!patient_id_o := case_when(!is.na(!!sym(chi_o)) ~ !!sym(chi_o),
                                           is.na(!!sym(chi_o)) & 
                                             !is.na(!!sym(upi_o)) ~ !!sym(upi_o)),
           .after=!!ucpn_o
           )
  return(df_with_patient_id)
  
}
