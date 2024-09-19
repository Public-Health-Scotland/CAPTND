###########################################.
### Check data tables for IG compliance ###.
###########################################.

#Author: Bex Madden
#Date: 06/08/2024

# read in files used for publication's accompanying data tables

referrals <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/referrals_quarter_hb.parquet"))
ref_accept <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance/non_acceptance_summary_quarter_hb.parquet"))
appointments <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet"))

check_ig_compliance <- function(df){
  df <- df |> 
    select(-contains("prop")) |> 
    filter(if_any(where(is.numeric), ~ .x <= 5))
  
  if(nrow(df) > 0){
  return(df)
  }else{
    return(print("No IG concerns"))
  }
}

referrals_check <- check_ig_compliance(referrals)

ref_accept_check <- check_ig_compliance(ref_accept)

appointments_check <- check_ig_compliance(appointments)

