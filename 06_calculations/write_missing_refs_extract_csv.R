##################################################################.
### Write extract for patient pathways without referral record ###
##################################################################.

# Author: Luke Taylor
# Date: 2025-06-16

# Identifies all patient pathways for which an appointment record has been submitted in the last 15 months,
# that lacks either a ref_date or ref_rec_date

write_missing_ref_extract <- function(HB, dataset_choice){

df_glob_swift_completed_rtt <- read_parquet(paste0(root_dir,'/swift_glob_merged.parquet'))

df_check <- df_glob_swift_completed_rtt |>
  filter(hb_name == HB,
         dataset_type == dataset_choice) |>
  group_by(dataset_type, hb_name, ucpn, patient_id) |>
  mutate(has_ref_record = fcase(any(!is.na(ref_date) | any(!is.na(ref_rec_date))), TRUE,
                              default = FALSE)) |>
  mutate(last_twelve_mths = case_when(any(app_date >= month_end - months(15)) ~ 'Seen',
                                      TRUE ~ NA)) |>
  arrange(ucpn, app_date) |>
  filter(has_ref_record == FALSE,
         last_twelve_mths == 'Seen') |>
  select(dataset_type, hb_name, ucpn, patient_id) |>
  distinct() |>
  write_csv(paste0("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/RTT_testing/missing_refs_extract/",
                   HB, "_" , dataset_choice, "_", month_end, "_", "missing_refs_extract.csv"))

}


#total missing referrals by dataset and HB with activity in the last 15 months
# hb_totals <- df_glob_swift_completed_rtt |>
#   group_by(dataset_type, hb_name, ucpn, patient_id) |>
#   mutate(has_ref_record = fcase(any(!is.na(ref_date) | any(!is.na(ref_rec_date))), TRUE,
#                                 default = FALSE)) |>
#   mutate(last_twelve_mths = case_when(any(app_date >= month_end - months(15)) ~ 'Seen',
#                                       TRUE ~ NA)) |>
#   arrange(ucpn, app_date) |>
#   filter(has_ref_record == FALSE,
#          last_twelve_mths == 'Seen') |>
#   select(dataset_type, hb_name, ucpn, patient_id) |>
#   distinct() |>
#   group_by(dataset_type, hb_name) |>
#   summarise(count = n())


