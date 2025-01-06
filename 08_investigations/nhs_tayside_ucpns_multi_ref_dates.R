
#######################################################################.
### Check same UCPN with multiple referral dates within NHS Tayside ###
#######################################################################.

# Author: Charlie Smith
# Date: 2024-11-25 


df_opti <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 

df_check <- df_opti |> 
  select(hb_name, header_month, dataset_type, ucpn, ref_rec_date_opti) |> 
  filter(hb_name == "NHS Tayside" &
           header_month %in% c(seq.Date(from = ymd("2024-01-01"), to = ymd("2024-12-01"), by = "month"))) |> 
  arrange(ucpn, ref_rec_date_opti) |> 
  select(-header_month) |> 
  distinct() |> 
  group_by(dataset_type, ucpn, ref_rec_date_opti) |>
  mutate(count = row_number())
  
  
# result: there are no UCPN with multiple referral dates within NHS Tayside  

