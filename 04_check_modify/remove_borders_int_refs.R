##################################################################.
### Function - Remove Internal CAMHS Referrals for NHS Borders ###
##################################################################.

# Author: Luke Taylor
# Date: 2025-04-08

#An internal referral is created for every CAMHS patient referred to PT in NHS Borders
#These patients are therefore double counted
#This function flags these patients, which can be identified as they have the referral source '06' CAMHS

remove_borders_int_refs <- function(df){

df_borders_refs <- df |>
  mutate(flag = case_when(!!sym(hb_name_o) == 'NHS Borders' & 
                            !!sym(ref_source_o) == 6 &
                            !!sym(dataset_type_o) == 'CAMHS' ~ 1,
                          TRUE ~ 0)) |>
  filter(flag != 1) |>
  select(-flag)

  return(df_borders_refs)

}
