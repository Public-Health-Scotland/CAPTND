
###########################.
### D&G UCPN Update Fix ###
###########################.

# Author: Charlie Smith
# Date: 2024-09-13

# Issue: D&G PT moving to Morse from early October 2024, meaning that PT will adopt new UCPNs. 
# This will mean that pre-existing records' UCPNs will no longer match. Phil will
# provide the old-style UCPN in the UPI field. 

# Solution: for submission from 2024-10-24 onwards, ensure old UCPN (in UPI field)
# for D&G pathways IF pathway has info from prior 2024-10-24.

#Update: D&G did not provide old UCPN in UPI field, so a lookup table has been provided
#which should contain all patients affected by the migration to Morse.

dumfries_ucpn_fix <- function(df){
  
  dag_ucpn_fix <- read.xlsx("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/dumfries_morse_migration_lookup.xlsx") |>
    janitor::clean_names() |>
    mutate(across(everything(), as.character))
  
  df_fixed <- df |>
    
    left_join(dag_ucpn_fix, by = c("patient_id" = "patient_chi", "ucpn" = "morse_ucpn")) |>
    
    group_by(across(all_of(data_keys))) |> # group by pathway
    
    mutate(!!ucpn_o := case_when( # overwrite ucpn...
      
      !!sym(hb_name_o) == "NHS Dumfries and Galloway" & !!sym(dataset_type_o) == "PT" & # for NHS D&G PT
        !!sym(ucpn_o) != migrated_from_ucpn # when ucpn and upi do not match
               ~ migrated_from_ucpn, # use old ucpn in place of new ucpn 
      
      TRUE ~ !!sym(ucpn_o))) |> # otherwise keep ucpn as it is
  
  # eventually the ucpns and upis will match again for fresh referrals that have only been recorded in Morse
    
    ungroup()
  
  message('D&G UCPN issue from Morse switchover fixed (review necessity periodically)\n')
  
  return(df_fixed)
  
}

