

date_min <- most_recent_month_in_data %m-% years(1)
date_max <- most_recent_month_in_data

#opti data
df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

df_opti <- df |>
  mutate(sub_month = floor_date(!!sym(header_date_o), unit = "month")) |>
  filter(sub_month <= date_max,
         sub_month > date_min) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), sub_month) |>
  summarise(opti_count = n()) |>
  group_by(!!sym(dataset_type_o), sub_month) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop"))

#raw data
df_swift_clean <- read_parquet(paste0(root_dir, "/swift_extract.parquet"))

df_raw <- df_swift_clean |>
  mutate(sub_month = floor_date(!!sym(header_date_o), unit = "month")) |>
  filter(sub_month <= date_max,
         sub_month > date_min,
         hb_name != 'NHS24') |>
  group_by(dataset_type, hb_name, sub_month) |>
  summarise(raw_count = n()) |>
  group_by(!!sym(dataset_type_o), sub_month) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop"))

df_compare <- df_opti |>
  left_join(df_raw, by = c("dataset_type", "hb_name", "sub_month")) |>
  mutate(count_diff = raw_count - opti_count,
         perc_retained = round(opti_count/raw_count*100, 1),
         sub_month = as.Date(sub_month))


#appt records lost due to missing referral
df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 
month_start <- as.Date("2026-08-01")

appts_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         !is.na(app_date),
         header_date == month_start) |>
  select(dataset_type, hb_name, ucpn, chi, app_date, app_purpose, header_date) |>
  #distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  group_by(dataset_type, hb_name) |>
  summarise(appts_missing_ref = n()) |>
  mutate(sub_month = month_start)

#discharge records lost due to missing referral
dis_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         !is.na(case_closed_date),
         header_date == month_start) |>
  select(dataset_type, hb_name, ucpn, chi, case_closed_date, header_date) |>
  #distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  group_by(dataset_type, hb_name) |>
  summarise(dis_missing_ref = n()) |>
  mutate(sub_month = month_start)

#all other records lost due to missing referral
all_other_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         is.na(case_closed_date) & is.na(app_date),
         header_date == month_start) |>
  #distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  group_by(dataset_type, hb_name) |>
  summarise(all_other_missing_ref = n()) |>
  mutate(sub_month = month_start)

#join onto data retention df
df_compare <- df_compare |>
  left_join(appts_missing_refs, by = c('dataset_type', 'hb_name', 'sub_month')) |>
  left_join(dis_missing_refs, by = c('dataset_type', 'hb_name', 'sub_month')) |>
  left_join(all_other_missing_refs, by = c('dataset_type', 'hb_name', 'sub_month'))

missing_data_keys <- read.csv("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_2026-09-25/data_quality_basic/data_removed/removed_data_export/swift_removed_missing_ucpn_chi_upi_details.csv") |>
  mutate(header_date = as.Date(header_date)) |>
  filter(header_date == month_start) |>
  group_by(dataset_type, hb_name) |>
  summarise(missing_ucpn_chi_upi_n = n()) |>
  mutate(sub_month = month_start)

multi_ref_pathways <- read.csv("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_2026-09-25/data_quality_basic/data_removed/removed_data_export/swift_removed_multi_ref_path_details.csv") |>
  mutate(header_date = as.Date(header_date)) |>
  filter(header_date == month_start) |>
  group_by(dataset_type, hb_name) |>
  summarise(multi_ref_pathways_n = n()) |>
  mutate(sub_month = month_start)

non_unique_upi <- read.csv("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output/analysis_2026-09-25/data_quality_basic/data_removed/removed_data_export/swift_removed_non_unique_upi_details.csv") |>
  mutate(header_date = as.Date(header_date)) |>
  filter(header_date == month_start) |>
  group_by(dataset_type, hb_name) |>
  summarise(non_unique_upi_n = n()) |>
  mutate(sub_month = month_start)

#join removed pathways to df count
df_compare_complete <- df_compare |>
  left_join(missing_data_keys, by = c('dataset_type', 'hb_name', 'sub_month')) |>
  left_join(multi_ref_pathways, by = c('dataset_type', 'hb_name', 'sub_month')) |>
  left_join(non_unique_upi, by = c('dataset_type', 'hb_name', 'sub_month')) |>
  filter(sub_month == month_start,
         hb_name != 'NHS Scotland') |>
  mutate(removal_count = rowSums(across(c("appts_missing_ref", "dis_missing_ref", "all_other_missing_ref",
                                          "missing_ucpn_chi_upi_n", "multi_ref_pathways_n", "non_unique_upi_n")), na.rm = TRUE)) |>
  relocate(removal_count, .after = perc_retained) |>
  write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/captnd_lost_record_breakdown.xlsx")
  




#appt records lost due to missing referral
df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 
month_start <- as.Date("2026-08-01")

appts_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  group_by(dataset_type, hb_name, ucpn) |>
  mutate(is_ref_record = !is.na(ref_date) | !is.na(ref_rec_date),
         has_ref_record_ucpn = any(is_ref_record),
         chi_missing_on_ref = any(is_ref_record & is.na(chi)),
         n_chi_per_ucpn = n_distinct(chi, na.rm = TRUE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         !is.na(app_date),
         header_date == month_start) |>
  select(dataset_type, hb_name, ucpn, chi, app_date, app_purpose, header_date,
         has_ref_record_ucpn, chi_missing_on_ref, n_chi_per_ucpn) |>
  #distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  mutate(sub_month = month_start)

#ucpn has referral
#will be TRUE if a referral record exists for the UCPN but the CHI on the original referral record 
#does not match the CHI on the flagged appointment record

appt_has_ref_record_ucpn <- appts_missing_refs |>
  group_by(has_ref_record_ucpn, dataset_type, hb_name) |>
  summarise(n_has_ref_record_ucpn = n()) |>
  arrange(dataset_type, hb_name)

#referral missing chi
#will be TRUE if a referral record exists for the UCPN but the CHI was submitted as NA

appt_chi_missing_on_ref <- appts_missing_refs |>
  group_by(chi_missing_on_ref, dataset_type, hb_name) |>
  summarise(n_chi_missing_on_ref = n()) |>
  arrange(dataset_type, hb_name)

#number of chi per ucpn
#provides a count of the number of CHIs provided for the same UCPN

appt_chi_per_ucpn <- appts_missing_refs |>
  group_by(n_chi_per_ucpn, dataset_type, hb_name) |>
  summarise(chi_per_ucpn = n()) |>
  arrange(dataset_type, hb_name)



#discharge records lost due to missing referral
dis_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  group_by(dataset_type, hb_name, ucpn) |>
  mutate(is_ref_record = !is.na(ref_date) | !is.na(ref_rec_date),
         has_ref_record_ucpn = any(is_ref_record),
         chi_missing_on_ref = any(is_ref_record & is.na(chi)),
         n_chi_per_ucpn = n_distinct(chi, na.rm = TRUE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         !is.na(case_closed_date),
         header_date == month_start) |>
  select(dataset_type, hb_name, ucpn, chi, case_closed_date, header_date) |>
  #distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  mutate(sub_month = month_start)

#ucpn has referral
#will be TRUE if a referral record exists for the UCPN but the CHI on the original referral record 
#does not match the CHI on the flagged appointment record

dis_has_ref_record_ucpn <- dis_missing_refs |>
  group_by(has_ref_record_ucpn, dataset_type, hb_name) |>
  summarise(n_has_ref_record_ucpn = n()) |>
  arrange(dataset_type, hb_name)

#referral missing chi
#will be TRUE if a referral record exists for the UCPN but the CHI was submitted as NA

dis_chi_missing_on_ref <- dis_missing_refs |>
  group_by(chi_missing_on_ref, dataset_type, hb_name) |>
  summarise(n_chi_missing_on_ref = n()) |>
  arrange(dataset_type, hb_name)

#number of chi per ucpn
#provides a count of the number of CHIs provided for the same UCPN

dis_chi_per_ucpn <- dis_missing_refs |>
  group_by(n_chi_per_ucpn, dataset_type, hb_name) |>
  summarise(chi_per_ucpn = n()) |>
  arrange(dataset_type, hb_name)



#all other records lost due to missing referral
all_other_missing_refs <- df |>
  mutate(ucpn = str_replace_all(!!sym(ucpn_o), "\t", "")) |>
  group_by(dataset_type, hb_name, ucpn, chi) |>
  mutate(has_ref_record_pathway = fcase(any(!is.na(ref_date)) | any(!is.na(ref_rec_date)), TRUE,
                                        default = FALSE)) |> ungroup() |>
  group_by(dataset_type, hb_name, ucpn) |>
  mutate(is_ref_record = !is.na(ref_date) | !is.na(ref_rec_date),
         has_ref_record_ucpn = any(is_ref_record),
         chi_missing_on_ref = any(is_ref_record & is.na(chi)),
         n_chi_per_ucpn = n_distinct(chi, na.rm = TRUE)) |> ungroup() |>
  filter(has_ref_record_pathway == FALSE,
         is.na(case_closed_date) & is.na(app_date),
         header_date == month_start) |>
  #distinct() |>
  filter(!is.na(ucpn) & ucpn != "0" & ucpn != "NULL",
         !is.na(chi) & chi != "0" & chi != "NULL") |>
  arrange(dataset_type, hb_name, ucpn, app_date) |>
  mutate(sub_month = month_start)

#ucpn has referral
#will be TRUE if a referral record exists for the UCPN but the CHI on the original referral record 
#does not match the CHI on the flagged appointment record

misc_has_ref_record_ucpn <- all_other_missing_refs |>
  group_by(has_ref_record_ucpn, dataset_type, hb_name) |>
  summarise(n_has_ref_record_ucpn = n()) |>
  arrange(dataset_type, hb_name)

#referral missing chi
#will be TRUE if a referral record exists for the UCPN but the CHI was submitted as NA

misc_chi_missing_on_ref <- all_other_missing_refs |>
  group_by(chi_missing_on_ref, dataset_type, hb_name) |>
  summarise(n_chi_missing_on_ref = n()) |>
  arrange(dataset_type, hb_name)

#number of chi per ucpn
#provides a count of the number of CHIs provided for the same UCPN

misc_chi_per_ucpn <- all_other_missing_refs |>
  group_by(n_chi_per_ucpn, dataset_type, hb_name) |>
  summarise(chi_per_ucpn = n()) |>
  arrange(dataset_type, hb_name)


