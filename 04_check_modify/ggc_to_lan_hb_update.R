##############################.
### GGC to Lanarkshire Fix ###
##############################.

# Author: Luke Taylor
# Date: 2025-01-31

# Issue: Boundary changes mean that CamGlen is now served by NHS Lanarkshire rather than GGC. IT systems remains
# EMIS for this area, and Lanarkshire cannot get an data extract from this. Was agreed that GGC would submit 
#CamGlen data with hb_name as 'NHS Lanarkshire'. Appointment data for patients referred before this change in 
#submission practice are been chucked out by data quality checks.

# Solution: to change hb_name to NHS Lanarkshire for case now submitted as such by GGC, preventing loss of appt
#data.


ggc_to_lan_hb_update <- function(df){
  
  df_ggc_lan_update <- df |>
    
  group_by(!!sym(dataset_type_o), !!sym(ucpn_o), !!sym(patient_id_o)) |> #data keys except hb_name
    
  mutate(n_hb = n_distinct(!!sym(hb_name_o))) |> #count number of health boards per pathway
    
  mutate(!!hb_name_o := case_when( #overwrite hb_name when
    
    !!sym(hb_name_o) == 'NHS Greater Glasgow and Clyde'     #hb_name is GGC and,
    & n_hb == 2                                    #number of health boards per pathway is 2 and,
    & !!sym(dataset_type_o) == 'PT' ~ 'NHS Lanarkshire',    #dataset_type is PT
                             TRUE ~ !!sym(hb_name_o))) |>   #replace with NHS Lanarkshire
  select(-n_hb) |>
  ungroup()
  
  return(df_ggc_lan_update)
  
}
  