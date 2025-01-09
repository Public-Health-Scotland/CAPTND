
##########################.
### Get DQ proportions ###
##########################.

# Author: Charlie Smith
# Date: 2024-05-08


get_dq_proportions <- function(df){
  
  df_prop <- df %>% 
    ungroup() %>% 
    group_by(header_date_month, dataset_type, hb_name, variable) %>% 
    mutate(total = sum(count), 
           proportion = round(count / total * 100, digits = 1)) %>% 
    ungroup() 
  
  return(df_prop)
  
}
