
#############################################.
### Append Age at Referral and Age Groups ###
#############################################. 

# Author: Charlie Smith
# Date: 2023-08-14

# library(phsmethods)

append_age_vars <- function(df){
  
  df_age <- df %>% 
    group_by(!!sym(patient_id_o)) %>% 
    mutate(!!sym(age_at_ref_rec_o) := age_calculate(start = !!sym(dob_verified_o), 
                                                    end = !!sym(ref_rec_date_o), 
                                                    units = "years", round_down = TRUE),
           !!sym(age_group_o) := create_age_groups(x = !!sym(age_at_ref_rec_o), 
                                                   from = 0, to = 90, by = 5),
           .after=!!sym(dob_verified_o))  # is "by = 5" suitable for CAMHS? 
  
  return(df_age)
  
}

