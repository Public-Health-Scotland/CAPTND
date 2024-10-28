
###########################.
### D&G UCPN Update Fix ###
###########################.

# Author: Charlie Smith
# Date: 2024-09-13

# Issue: D&G PT moving to Morse from early October 2024, meaning that PT will adopt a UCPN. 
# This will mean that pre-existing records' UCPNs will no longer match. Phil will
# provide the old-style UCPN in the UPI field. 

# Solution: for submission from 2024-10-24 onwards, ensure old UCPN (in UPI field)
# for D&G pathways IF pwathway has info from prior 2024-10-24.

dumfries_ucpn_fix <- function(df){
  
  df_fixed <- df |>
    
    group_by(across(all_of(data_keys))) |> # group by pathway (since upis can be across multiple ucpns)
    
    mutate(!!ucpn_o := case_when( # overwrite ucpn...
      
      !!sym(hb_name_o) == "NHS Dumfries and Galloway" & !!sym(dataset_type_o) == "PT" & # for NHS D&G PT
        !!sym(ucpn_o) != !!sym(upi_o) # when ucpn and upi do not match
               ~ !!sym(upi_o), # use old ucpn (upi) in place of new ucpn 
      
      TRUE ~ !!sym(ucpn_o))) |> # otherwise keep ucpn as it is
  
  # eventually the ucpns and upis will match again for fresh referrals that have only been recorded in Morse
    
    ungroup()
  
  return(df_fixed)
  
}

