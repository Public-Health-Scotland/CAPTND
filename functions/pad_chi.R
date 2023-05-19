


pad_chi <- function(df){
  
  df_padded <- df %>% 
    mutate(!!chi_o := as.character(!!sym(chi_o)),
           !!chi_o := chi_pad(!!sym(chi_o)))
  
  return(df_padded)
  
}




