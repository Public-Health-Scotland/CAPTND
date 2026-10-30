##############################################
####   Cross board movement of patients   #### 
##############################################

#Author: Luke Taylor
#Date: 14/10/2026

#df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

#hscp lookup file
hb_hscp_lookup <- readRDS('/conf/linkage/output/lookups/Unicode/Geography/HSCP Locality/HSCP Localities_DZ11_Lookup_20240513.rds') |>
  select(hscp2019name, hb2019name) |>
  distinct()

df_cross_board_full <- df |>
  filter(!is.na(chi)) |>
  group_by(dataset_type, chi) |>
  mutate(hb_n = n_distinct(hb_name)) |> ungroup() |> #determine patients that have referral to more than one different hb
  filter(hb_n >= 2) |> #filter to keep only patients who have been referred to more than one hb
  arrange(chi, hb_name) |>
  select(dataset_type, hb_name, chi, upi, ucpn, postcode_last_reported, ref_rec_date_opti, ref_acc_opti, 
         app_date, app_purpose, att_status, act_code_sent_date, case_closed_date, hb_n, hscp2019name,
         prof_group) |>
  left_join(hb_hscp_lookup, by = "hscp2019name") |> #join lookup to determine hb of residence
  group_by(dataset_type, hb_name, chi, ucpn) |>
  mutate(most_recent_app_date = if_else(any(!is.na(app_date)),
                                        max(app_date, na.rm = TRUE),
                                        as.Date(NA))) |> #create new column with date of most recently attended appt for each pathway
  ungroup()

df_cross_board_filtered <- df_cross_board_full |>
  select(dataset_type, treatment_hb = hb_name, chi, ref_rec_date_opti, 
         referral_status = ref_acc_opti, residence_hb = hb2019name, most_recent_app_date) |>
  distinct() |>
  arrange(chi, ref_rec_date_opti)


########################### Continuation of care analysis ###########################

#patients receiving treatment outwith hb of residence by dataset type and referral year
hb_treat_res_df <- df_cross_board_filtered |>
  mutate(year = lubridate::year(ref_rec_date_opti), #year of referral
         referral_status = case_when(referral_status == 1 ~ 'Accepted',
                                     TRUE ~ 'Not accepted')) |> #acceptance status 
  select(dataset_type, chi, treatment_hb, residence_hb, referral_status, ref_rec_date_opti, year) |>
  filter(treatment_hb != residence_hb) #filter to keep only patients where hb of residence is not the same as hb of treatment

hb_treat_res_agg_count <- hb_treat_res_df |>
  group_by(dataset_type, year, treatment_hb, residence_hb, referral_status) |>
  summarise(patient_count = n()) |>
  ungroup()

df_split_treat_res_df <- hb_treat_res_df |>
  split(hb_treat_res_df$year)

openxlsx::write.xlsx(df_split_treat_res_df,
                      file = "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/captnd_cross_border_flow.xlsx",
                      overwrite = TRUE)

#location of appointment lookup
loc_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Location") %>% 
  rename(location = Code,
         loc_label = Location) |>
  mutate(loc_label = str_to_sentence(loc_label),
         loc_label = case_when(loc_label == 'Attend anywhere e.g. skype call,  ecbt' ~ 'Attend anywhere',
                               loc_label == 'Public place e.g. street, third sector' ~ 'Public place',
                               loc_label == 'Nhs near me' ~ 'NHS Near Me',
                               TRUE ~ loc_label)) 

#Is being treated outside of health board of residence reflected in the location of appointments recorded for these patients
pat_lookup <- hb_treat_res_df |>
  select(dataset_type, chi, treatment_hb, ref_rec_date_opti, residence_hb) |>
  distinct() |>
  mutate(flag = 1)

appt_loc_agg_df <- df |>
  left_join(pat_lookup, by = c("dataset_type", "chi", "hb_name" = "treatment_hb", "ref_rec_date_opti")) |>
  filter(flag == 1) |>
  select(dataset_type, chi, ucpn, hb_name, residence_hb,
         ref_rec_date_opti, ref_acc_opti, app_date, att_status, location) |>
  left_join(loc_lookup, by = "location")

appt_loc_grp_count <- appt_loc_agg_df |>
  select(dataset_type, chi, hb_name, loc_label) |>
  distinct() |>
  group_by(loc_label) |>
  summarise(count = n()) |> ungroup() |>
  arrange(desc(count)) |>
  filter(!is.na(loc_label)) |>
  mutate(total = sum(count),
         perc = round(count/total*100, 1))
  
########################### Continuation of care analysis ###########################

#patients that were referred to different health boards within 6 months of last appointment attendance
multi_hb_refs_6mth <- df_cross_board_filtered |>
  group_by(dataset_type, chi) |>
  mutate(six_mth_post_prev_ref = lag(ref_rec_date_opti + months(6)), #lag six month date post previous referral
         prev_treat_hb = lag(treatment_hb), #lag previous hb of treatment name
         prev_ref_rec_date = lag(ref_rec_date_opti), #lag prev ref rec date
         six_mth_post_last_att_for_prev_ref = lag(most_recent_app_date + months(6)), #lag six month date post most recent appointment date
         referral_status = case_when(referral_status == 1 ~ 'Accepted', #add accepted or not accepted label
                                     TRUE ~ 'Not accepted')) |> ungroup() |>
  select(dataset_type, chi, treatment_hb, ref_rec_date_opti, residence_hb, referral_status,
         prev_treat_hb, six_mth_post_prev_ref, six_mth_post_last_att_for_prev_ref) |>
  filter(treatment_hb != prev_treat_hb,
         ref_rec_date_opti <= six_mth_post_prev_ref | 
           ref_rec_date_opti <= six_mth_post_last_att_for_prev_ref) |> #if referral was within 6mths of last activity for
  mutate(year = lubridate::year(ref_rec_date_opti))                    #previous referral, can we assume this is continuation
                                                                       #of care?

most_common_hb_switch <- multi_hb_refs_6mth |>
  group_by(dataset_type, year, prev_treat_hb, treatment_hb) |>
  summarise(count = n())

df_split_treat_con <- most_common_hb_switch |>
  split(most_common_hb_switch$year)

openxlsx::write.xlsx(df_split_treat_con,
                     file = "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/data/captnd_treat_continuation.xlsx",
                     overwrite = TRUE)
  
  
#number of patients receiving care across different health boards
df_no_of_pat <- df_cross_board_filtered |>
  select(chi) |> distinct() |>
  summarise(count = n())

#number of patients receiving care in different hb within 6 mths of referral or last appointment date at previous hb
df_no_of_pat_6mth <- multi_hb_refs_6mth |>
  select(chi) |> distinct() |>
  summarise(count = n())


#Question: Is there a reason why patients are being treated outside of their health board of residence
# within 6 months of their last scheduled appointment or referral with the previous health board?

#Could it be that the specialist treatment they require does not exist in thier own board?

#Check whether prof group is related to treatment outwith health board
prof_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Pro_Group") %>% 
  rename(prof_group = Code,
         prof_label = Pro_Group) |>
  mutate(prof_label = str_to_sentence(prof_label),
         prof_label = case_when(prof_label == 'Cbt therapist' ~ 'CBT therapist',
                                TRUE ~ prof_label)) 

#patients treated in health board outside of their health board of residence referred within
#6 months of either their previous referral or the last scheduled appointment with the previous board
hb_differ_treat_res_refs_6mth <- df_cross_board_filtered |>
  group_by(dataset_type, chi) |>
  mutate(six_mth_post_prev_ref = lag(ref_rec_date_opti + months(6)), #lag six month date post previous referral
         prev_treat_hb = lag(treatment_hb), #lag previous hb of treatment name
         prev_ref_rec_date = lag(ref_rec_date_opti), #lag prev ref rec date
         six_mth_post_last_att_for_prev_ref = lag(most_recent_app_date + months(6)), #lag six month date post most recent appointment date
         referral_status = case_when(referral_status == 1 ~ 'Accepted', #add accepted or not accepted label
                                     TRUE ~ 'Not accepted')) |> ungroup() |>
  select(dataset_type, chi, treatment_hb, ref_rec_date_opti, residence_hb, referral_status,
         prev_treat_hb, six_mth_post_prev_ref, six_mth_post_last_att_for_prev_ref) |>
  filter(treatment_hb != residence_hb,
         ref_rec_date_opti <= six_mth_post_prev_ref | 
           ref_rec_date_opti <= six_mth_post_last_att_for_prev_ref) |> #if referral was within 6mths of last activity for
  mutate(year = lubridate::year(ref_rec_date_opti))

#should it be within 6 months of last attended appointment, rather than last scheduled appointment?

#patient lookup
df_prof_grp_lookup <- hb_differ_treat_res_refs_6mth |>
  select(dataset_type, treatment_hb, chi, ref_rec_date_opti, referral_status, residence_hb) |>
  distinct() |>
  mutate(flag = 1)

prof_grp_of_pat_ref <- df |>
  left_join(df_prof_grp_lookup, by = c("dataset_type", "chi", "hb_name" = "treatment_hb",
                                       "ref_rec_date_opti")) |>
  filter(flag == 1) |>
  select(dataset_type, chi, ucpn, hb_name, residence_hb, ref_rec_date_opti, ref_acc_opti, app_date,
         att_status, prof_group) |>
  left_join(prof_lookup, by = "prof_group")

  
prof_grp_count <- prof_grp_of_pat_ref |>
  select(dataset_type, chi, hb_name, prof_label) |>
  distinct() |>
  group_by(prof_label) |>
  summarise(count = n())
  

