hb <- read_parquet("../../../output/removed/remove_unusable_records_2023-06-29 13:17:21.parquet")
#ur <- read_parquet("../../../output/removed/remove_unusable_records_swift_2023-06-29.parquet")
ur <- read_parquet("../../../output/removed/remove_unusable_chi_swift_2023-06-29.parquet")

ur2 <- ur %>% 
  filter((location== '01'|location== '1') & is.na(chi))

ur3 <- ur %>% 
  filter((location== '01'|location== '1') & is.na(ucpn))

ur4 <- ur %>% 
  filter(!is.na(ref_rec_date) & !is.na(ref_date) & !is.na(ucpn) & !is.na(dob)
         & !is.na(postcode) & is.na(chi) & is.na(app_date) & is.na(case_closed_date))

ur_lan <- ur %>% 
  filter(str_detect(hb_name,"Lanark")) %>% 
  filter(is.na(chi))

ur_no_chi <- ur %>% 
  filter(is.na(chi))

no_chi <- swift_all %>% 
  filter(is.na(chi))



