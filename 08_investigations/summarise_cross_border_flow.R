##############################################
####   Cross board movement of patients   #### 
##############################################

#Author: Luke Taylor
#Date: 23/11/2026

#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

month_end <- as.Date("2026-09-01")
month_start <- lubridate::`%m-%`(as.Date(month_end), lubridate::period(months = 14))

#% of patients treated in their own health board area
#hscp lookup file
hb_hscp_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.rds') |>
  select(hscp2019name, hb_residence = hb2019name) |>
  distinct()

df_hb_res_treat <- df |>
  filter(!is.na(first_treat_app),
         first_treat_app >= month_start) |> # apply date range filter
  lazy_dt() |> 
  group_by(dataset_type, hb_name, ucpn, patient_id) |> 
  slice(1) |> ungroup() |>
  as.data.frame() |> 
  select(dataset_type, hb_treatment = hb_name, chi, upi, ucpn, postcode_last_reported, ref_rec_date_opti, ref_acc_opti, 
         app_date, app_purpose, att_status, act_code_sent_date, first_treat_app, case_closed_date, hscp2019name) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |> #join lookup to determine hb of residence
  group_by(dataset_type, hb_residence, hb_treatment) |>
  summarise(count = n()) |>
  group_by(dataset_type, hb_residence) |>
  arrange(dataset_type, hb_residence, desc(count)) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 2)) %>%
  bind_rows(summarise(.,
                      across(count, sum),
                      across(hb_treatment, ~"Total"),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  mutate(rank = row_number()) |> ungroup() #|>
  #write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/patient_flow_hb_res_treat_start.xlsx")
 
#% of patients referred to their own health board area
df_hb_res_ref <- df |>
  filter(ref_rec_date_opti >= month_start) |> # apply date range filter
  lazy_dt() |> 
  group_by(dataset_type, hb_name, ucpn, patient_id) |> 
  slice(1) |> ungroup() |>
  as.data.frame() |> 
  select(dataset_type, hb_treatment = hb_name, chi, upi, ucpn, postcode_last_reported, ref_rec_date_opti, ref_acc_opti, 
         app_date, app_purpose, att_status, act_code_sent_date, first_treat_app, case_closed_date, hscp2019name) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |> #join lookup to determine hb of residence
  group_by(dataset_type, hb_residence, hb_treatment) |>
  summarise(count = n()) |>
  group_by(dataset_type, hb_residence) |>
  arrange(dataset_type, hb_residence, desc(count)) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 2)) %>%
  bind_rows(summarise(.,
                      across(count, sum),
                      across(hb_treatment, ~"Total"),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  mutate(rank = row_number()) |> ungroup() #|>
  #write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/patient_flow_hb_res_refs.xlsx")


#% of patients treated in other health board areas
df_hb_treat_treat <- df |>
  filter(!is.na(first_treat_app),
         first_treat_app >= month_start) |> # apply date range filter
  lazy_dt() |> 
  group_by(dataset_type, hb_name, ucpn, patient_id) |> 
  slice(1) |> ungroup() |>
  as.data.frame() |> 
  select(dataset_type, hb_treatment = hb_name, chi, upi, ucpn, postcode_last_reported, ref_rec_date_opti, ref_acc_opti, 
         app_date, app_purpose, att_status, act_code_sent_date, first_treat_app, case_closed_date, hscp2019name) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |> #join lookup to determine hb of residence
  group_by(dataset_type, hb_treatment, hb_residence) |>
  summarise(count = n()) |>
  group_by(dataset_type, hb_treatment) |>
  arrange(dataset_type, hb_treatment, desc(count)) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 2)) %>%
  bind_rows(summarise(.,
                      across(count, sum),
                      across(hb_residence, ~"Total"),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  mutate(rank = row_number()) |> ungroup() #|>
  #write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/patient_flow_hb_treat_treat_start.xlsx")


#% of patients referred to other health board areas
df_hb_treat_ref <- df |>
  filter(ref_rec_date_opti >= month_start) |> # apply date range filter
  lazy_dt() |> 
  group_by(dataset_type, hb_name, ucpn, patient_id) |> 
  slice(1) |> ungroup() |>
  as.data.frame() |> 
  select(dataset_type, hb_treatment = hb_name, chi, upi, ucpn, postcode_last_reported, ref_rec_date_opti, ref_acc_opti, 
         app_date, app_purpose, att_status, act_code_sent_date, first_treat_app, case_closed_date, hscp2019name) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |> #join lookup to determine hb of residence
  group_by(dataset_type, hb_treatment, hb_residence) |>
  summarise(count = n()) |>
  group_by(dataset_type, hb_treatment) |>
  arrange(dataset_type, hb_treatment, desc(count)) |>
  mutate(total = sum(count),
         prop = round(count/total*100, 2)) %>%
  bind_rows(summarise(.,
                      across(count, sum),
                      across(hb_residence, ~"Total"),
                      across(prop, ~ 100),
                      .groups = "drop")) |>
  mutate(rank = row_number(),
         hb_residence = case_when(is.na(hb_residence) ~ 'Unknown residency',
                                  TRUE ~ hb_residence)) |> ungroup() #|>
  #write.xlsx("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/patient_flow_hb_treat_refs.xlsx")


append_quarter_ending <- function(df, date_col){
  
  df_quarters <- df |> 
    mutate(quarter = lubridate::ceiling_date(!!sym(date_col), unit = "quarter") - 1,
           quarter_ending = lubridate::floor_date(quarter, unit = "month")) |> 
    select(-quarter)
  
  return(df_quarters)
  
}

markdown_df <- df |>
  filter(ref_rec_date_opti >= month_start) |> # apply date range filter
  lazy_dt() |> 
  group_by(dataset_type, hb_name, ucpn, patient_id) |> 
  slice(1) |> ungroup() |>
  as.data.frame() |> 
  select(dataset_type, treatment_hb = hb_name, ref_rec_date_opti, hscp2019name) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |> #join lookup to determine hb of residence
  append_quarter_ending(date_col = "ref_rec_date_opti") |>
  select(dataset_type, treatment_hb, residence_hb = hb_residence, quarter_ending) |>
  group_by(dataset_type, treatment_hb, residence_hb, quarter_ending) |>
  mutate(residence_hb = case_when(is.na(residence_hb) ~ 'Unknown residency',
                                  TRUE ~ residence_hb),
         patient_count = n()) |>
  distinct() |> ungroup()


