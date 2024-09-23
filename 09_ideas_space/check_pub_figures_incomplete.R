
#################################.
### Check publication figures ###
#################################.

# Author: Charlie Smith
# Date: 2024-7-24

check_publication_figs <- function(){
  
  # Referrals
  df_refs <- read_parquet(
    paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", 
           data_analysis_latest_date, "/shorewise_publication/data/referrals/table_referrals_quarterly.parquet"))
  
  nc <- ncol(df_refs)
  
  df_refs <- df_refs |> 
    select(c(1, 2, nc)) |> 
    filter(`Health board` %in% c("NHSScotland", "NHS Scotland"))
  
  nc <- ncol(df_refs)
  
  # Accepted refs
  df_acc <- read_parquet(
    paste0("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_", 
           data_analysis_latest_date, "/shorewise_publication/data/non_acceptance/table_acc_rate.parquet")) |> 
    select(c(1, 2, Total)) |> 
    filter(`Health board` %in% c("NHSScotland", "NHS Scotland")) 
  
  
  # check refs == total in accepted refs
  df_check1 <- left_join(df_refs, df_acc, by = c("dataset_type", "Health board")) |> 
    mutate(match = , Total))
  
  
}




