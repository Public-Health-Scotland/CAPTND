
############################################################.
### Remove leading zeros from treatment/intervention 1-3 ###
############################################################.

# Author: Charlie Smith
# Date: 2024-12-19


remove_lead_0s_treat_int <- function(df){
  
  df_fixed <- df |> 
    mutate(!!sym(treat_1_o) := if_else(!!sym(treat_1_o) %in% c("096", "098", "099"),
                                      str_remove(!!sym(treat_1_o), "0"), 
                                      !!sym(treat_1_o)),
           !!sym(treat_2_o) := if_else(!!sym(treat_2_o) %in% c("096", "098", "099"),
                                       str_remove(!!sym(treat_2_o), "0"), 
                                       !!sym(treat_2_o)),
           !!sym(treat_3_o) := if_else(!!sym(treat_3_o) %in% c("096", "098", "099"),
                                       str_remove(!!sym(treat_3_o), "0"), 
                                       !!sym(treat_3_o))
           )
  
  return(df_fixed)
  
}


