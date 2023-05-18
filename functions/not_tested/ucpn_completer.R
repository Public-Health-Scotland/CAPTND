########################.
###   UCPN completer ###
########################.

#Completes UCPN according to rec date and CHI




complete_ucpn_recDate <- function(df){
  
  
  df_complete=df %>% 
    mutate(!!ref_rec_date_o := case_when(is.na(!!ref_rec_date_o) ~ !!ref_rec_date_o := !!ref_date_o,
                     TRUE ~ !!ref_rec_date_o)) %>%
    group_by(!!sym(chi_o), !!sym(ref_rec_date_o))  %>% 
    fill(!!ucpn_o, .direction="downup")
    
  
  return(df_complete)
}












