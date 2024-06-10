
##############################################.
### Check invalid treatement codes for D&G ###
##############################################.

# Author: Charlie Smith
# Date: 2024-06-05


df <- read_parquet('../../../../R script/CAPTND Data Prep/Output/2024-05-28/all_unique_records_checked_extra_demo_stage.parquet')

df_dumfries <- df |> 
  filter(HB == "NHS Dumfries and Galloway" &
           HEADER_REF_MONTH == "2024-04-01" &
           check_TREAT1 == "Invalid") |> 
  select(BATCH_MONTH, DATASET, HB, UCPN, CHI, TREAT1, check_TREAT1, TREAT2, 
         check_TREAT2, TREAT3, check_TREAT3)

df_treat1 <- df_dumfries |> 
  select(TREAT1) |> 
  distinct() |> 
  pull()
