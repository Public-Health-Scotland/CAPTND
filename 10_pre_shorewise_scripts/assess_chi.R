
##################.
### Assess CHI ###
##################.

# Author: Bex Madden
# Date: 2024-04-10

assess_chi <- function(df){
  
  df_chi <- df |> 
    mutate(!!chi_o := gsub("[[:blank:]]", "", !!sym(chi_o)),
           !!chi_o := if_else(nchar(!!sym(chi_o)) < 10, str_pad(!!sym(chi_o), 10, "left", "0"), !!sym(chi_o)),
           check_chi = case_when(
             is.na(!!sym(chi_o)) ~ "missing",
             substring(!!sym(chi_o), 1, 2) == '00' |
               as.numeric(substring(!!sym(chi_o), 1, 2)) > 31 |
               as.numeric(substring(!!sym(chi_o), 3, 4)) > 12 |
               grepl("[A-Za-z]", !!sym(chi_o)) |   #replacing substring(!!sym(chi_o), 1, nchar(!!sym(chi_o))) %in% c(letters, LETTERS)
               nchar(!!sym(chi_o)) != 10 ~ "invalid",
             TRUE ~ "valid"))
  
  return(df_chi)
  
}
