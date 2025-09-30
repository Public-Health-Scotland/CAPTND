######################################################################################
### Identify duplicate ref records that have been submitted with the same ref date ###
######################################################################################

#This script identifies referral records that have been submitted with the same ref date and ref rec date
#more than once. 

header_mth <- as.Date("2025-08-01")
#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

dup_referral_recs <- function(){
df_dup_refs <- df |>
  arrange(chi, ucpn) |>
  filter(!is.na(postcode)) |>
  group_by(hb_name, dataset_type, chi, ucpn, ref_date, ref_rec_date) |>
  mutate(n = n()) |>
  #keep pathways with more than one record for the same referral date
  filter(n >= 2) |>
  #keep only cases with a submission in the latest reporting month
  mutate(latest_mth = case_when(any(header_date == header_mth) ~ 1,
                                TRUE ~ NA)) |>
  filter(latest_mth == 1,
         hb_name != 'NHS24') |> #remove NHS24 cases
  group_by(hb_name, dataset_type, chi, ucpn, ref_acc) |>
  mutate(n = n()) |>
  #keep pathways with the same referral acceptance status on referral records
  #this helps remove records that have been submitted to update referral acceptance status
  filter(n != 1)

}


