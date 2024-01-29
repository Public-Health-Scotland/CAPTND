
#####################################.
### NHS Scotland referrals monthly###
#####################################.

# Author: Charlie Smith
# Date: 2023-11-17


get_referrals_monthly_sco <- function(){
  
  refs_monthly_sco <- read_parquet(paste0(data_working_safe, 'captnd_pub.parquet')) |> 
    # filter(!is.na(!!sym(ref_acc_o))) |> 
    select(all_of(data_keys), !!ref_acc_o, !!referral_month_o) |> 
    distinct() |> 
    group_by(!!!syms(c(referral_month_o, dataset_type_o))) |> 
    summarise(referrals = n(), .groups = 'drop') |> 
    save_as_parquet(path = paste0(data_working_safe, "refs_monthly_sco"))  
  
}

