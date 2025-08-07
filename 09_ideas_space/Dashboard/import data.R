###############################################################
################# Create CAPTND DT for Dashboard ##############
###############################################################

#Author: Luke Taylor
#Date: 05-05-2025

#Script to create datatables for Shiny dashboard

##### Referral demographics #####
ref_sex_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_sex.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by sex') |>
  rename(measure_breakdown = sex_reported) |>
  pivot_longer(cols = c('count', 'total', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_age_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_age.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by age') |>
  rename(measure_breakdown = agg_age_groups) |>
  pivot_longer(cols = c('count', 'total', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_simd_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_demographics/referrals_quarter_hb_simd.parquet")) |>
  select(-population, -tot_population, -tot_pop_rate_1000) |>
  mutate(measure_name = 'Referrals by SIMD') |>
  rename(measure_breakdown = simd2020_quintile) |>
  pivot_longer(cols = c('count', 'total', 'prop', 'pop_rate_1000'),
               names_to = 'measure_type',
               values_to = 'count')

ref_master_df <- rbind(ref_sex_df, ref_age_df, ref_simd_df)

##### Referral acceptance #####
ref_accept_df <- read_parquet(paste0(shorewise_pub_data_dir, "/non_acceptance/non_acceptance_summary_quarter_hb.parquet")) |>
  mutate(measure_name = 'Referrals acceptance status') |>
  rename(measure_breakdown = ref_acc_desc) |>
  pivot_longer(cols = c('count', 'total', 'prop'),
               names_to = 'measure_type',
               values_to = 'count')


##### Appointment attendance #####
appt_att_df <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |>
  mutate(measure_name = 'Appointment attendance status') |>
  rename(measure_breakdown = Attendance,
         Count = apps_att,
         Total = total_apps,
         Percentage = prop_apps_att) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### First contact attendance #####
first_con_df <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb.parquet")) |>
  select(-total_apps) |>
  mutate(measure_name = 'First contact attendance status') |>
  rename(measure_breakdown = Attendance,
         Count = firstcon_att,
         Total = first_contact,
         Percentage = prop_firstcon_att) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### Referral source #####
df_ref_source <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/ref_source_month_hb.parquet")) |>
  mutate(measure_name = 'Referral source') |>
  rename(measure_breakdown = ref_source_name,
         Count = count,
         Total = total,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### Appointment care locations #####
df_care_loc <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_loc/apps_loc_mth_hb.parquet")) |>
  mutate(measure_name = 'Appointment care location') |>
  rename(measure_breakdown = loc_label,
         Count = count,
         Total = total_apps,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### Care professionals #####
df_prof_group <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_prof/apps_prof_mth_hb.parquet")) |>
  mutate(measure_name = 'Care professional') |>
  rename(measure_breakdown = prof_label,
         Count = count,
         Total = total_apps,
         Percentage = prop) |>
  pivot_longer(cols = c('Count', 'Total', 'Percentage'),
               names_to = 'measure_type',
               values_to = 'count')

##### First contact #####

##### Waiting list #####

##### Patients seen #####

##### Open cases #####
