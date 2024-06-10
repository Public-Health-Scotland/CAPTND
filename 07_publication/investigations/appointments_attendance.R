##########################################.
#### APP ATTENDANCE - for publication ####.
##########################################.

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


#### SETUP #####
# set DS choice

#dataset_choice <- "CAMHS"

# load data

df_captnd <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(app_month = floor_date(app_date, unit = "month"),
         app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(app_quarter, unit = "month")) 

# set constants
most_recent_month_in_data <- get_lastest_month_end(df_captnd)

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# select vars
demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")

df_vars <- df_captnd |>
  select(all_of(data_keys), all_of(demographics), ref_acc, app_date, att_status, 
         att_cat, app_quarter_ending) |> 
  mutate(app_quarter_ending = as.Date(app_quarter_ending)) |>
  group_by(!!!syms(data_keys), app_date) |>
  slice(1)

# get number of appointments per day per patient
df_app <- df_captnd |>
  filter(app_month %in% date_range) |>
  select(all_of(data_keys), app_date, app_month) |> # app_date not sufficient need to account for multiples
  filter(!is.na(app_date)) |> 
  group_by(across(all_of(c(data_keys, app_month_o, app_date_o)))) |> 
  summarise(n_app_patient_same_day = n(), .groups = 'drop') |>
  distinct() |>
  
  # join in demographics and other vars
  left_join(df_vars, by = c("patient_id", "ucpn", "dataset_type", 
                            "hb_name", "app_date"))


####### ALL TIME ##########

dna_hb_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended"
  )) |>
  group_by(dataset_type, hb_name, attendance) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

dna_sco_all <- dna_hb_all |> 
  group_by(dataset_type, attendance) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

dna_both_all <- rbind(dna_hb_all, dna_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_alltime"))

rm(dna_hb_all, dna_sco_all)

#### First contact apps only - attendance status ####

first_att_hb_all <- df_app |>
  filter(att_cat == 1) |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "2" ~ "Clinic cancelled",
    att_status == "3" ~ "Patient cancelled",
    att_status == "5" ~ "Patient CNW",
    att_status == "8" ~ "Patient DNA",
    att_status == "9" ~ "Patient died",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not recorded"
  )) |>
  group_by(dataset_type, hb_name, attendance) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

first_att_sco_all <- first_att_hb_all |> 
  group_by(dataset_type, attendance) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

first_att_both_all <- rbind(first_att_hb_all, first_att_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_alltime"))

rm(first_att_hb_all, first_att_sco_all)


#### SEX - All apps attendance all time ####

dna_hb_sex_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended"
  ),
  Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) |>
  select(-sex_reported) |>
  group_by(dataset_type, hb_name, attendance, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

dna_sco_sex_all <- dna_hb_sex_all |> 
  group_by(dataset_type, attendance, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

dna_both_sex_all <- rbind(dna_hb_sex_all, dna_sco_sex_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_sex_alltime"))

rm(dna_hb_sex_all, dna_sco_sex_all)

#### SEX - First contact apps attendance all time ####

first_sex_hb_all <- df_app |>
  filter(att_cat == 1) |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "2" ~ "Clinic cancelled",
    att_status == "3" ~ "Patient cancelled",
    att_status == "5" ~ "Patient CNW",
    att_status == "8" ~ "Patient DNA",
    att_status == "9" ~ "Patient died",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not recorded"
  ),
  Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) |>
  select(-sex_reported) |>
  group_by(dataset_type, hb_name, attendance, Sex) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

first_sex_sco_all <- first_sex_hb_all |> 
  group_by(dataset_type, attendance, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

first_sex_both_all <- rbind(first_sex_hb_all, first_sex_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_sex_alltime"))

rm(first_sex_hb_all, first_sex_sco_all)


#### AGE - All apps attendance all time ####
dna_age_hb_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended")) |>
  group_by(dataset_type, hb_name, attendance, age_group) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

dna_age_sco_all <- dna_age_hb_all |> 
  group_by(dataset_type, attendance, age_group) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

dna_age_both_all <- rbind(dna_age_hb_all, dna_age_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, (readr::parse_number(age_group))) |> # orders age range) 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_age_alltime"))

rm(dna_age_hb_all, dna_age_sco_all)


#### AGE - First contact apps attendance all time ####
first_age_hb_all <- df_app |>
  filter(att_cat == 1) |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "2" ~ "Clinic cancelled",
    att_status == "3" ~ "Patient cancelled",
    att_status == "5" ~ "Patient CNW",
    att_status == "8" ~ "Patient DNA",
    att_status == "9" ~ "Patient died",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not recorded")) |>
  group_by(dataset_type, hb_name, attendance, age_group) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

first_age_sco_all <- first_age_hb_all |> 
  group_by(dataset_type, attendance, age_group) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

first_age_both_all <- rbind(first_age_hb_all, first_age_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, (readr::parse_number(age_group))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_age_alltime"))

rm(first_age_hb_all, first_age_sco_all)


#### SIMD - All apps attendance all time ####
dna_simd_hb_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended")) |>
  group_by(dataset_type, hb_name, attendance, simd2020_quintile) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

dna_simd_sco_all <- dna_simd_hb_all |> 
  group_by(dataset_type, attendance, simd2020_quintile) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

dna_simd_both_all <- rbind(dna_simd_hb_all, dna_simd_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, simd2020_quintile) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_simd_alltime"))

rm(dna_simd_hb_all, dna_simd_sco_all)


#### SIMD - First contact apps attendance all time ####
first_simd_hb_all <- df_app |>
  filter(att_cat == 1) |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "2" ~ "Clinic cancelled",
    att_status == "3" ~ "Patient cancelled",
    att_status == "5" ~ "Patient CNW",
    att_status == "8" ~ "Patient DNA",
    att_status == "9" ~ "Patient died",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not recorded")) |>
  group_by(dataset_type, hb_name, attendance, simd2020_quintile) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

first_simd_sco_all <- first_simd_hb_all |> 
  group_by(dataset_type, attendance, simd2020_quintile) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

first_simd_both_all <- rbind(first_simd_hb_all, first_simd_sco_all) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, simd2020_quintile) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_simd_alltime"))

rm(first_simd_hb_all, first_simd_sco_all)



####### QUARTERLY ##########

#### Total attended vs non-att apps #######

dna_hb <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended"
  )) |>
  group_by(dataset_type, hb_name, app_quarter_ending, attendance) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

dna_sco <- dna_hb |> 
  group_by(dataset_type, app_quarter_ending, attendance) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

dna_both <- rbind(dna_hb, dna_sco) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all"))

rm(dna_hb, dna_sco)


#### First contact apps only - attendance status ####

first_att_hb <- df_app |>
  filter(att_cat == 1) |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "2" ~ "Clinic cancelled",
    att_status == "3" ~ "Patient cancelled",
    att_status == "5" ~ "Patient CNW",
    att_status == "8" ~ "Patient DNA",
    att_status == "9" ~ "Patient died",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not recorded"
  )) |>
  group_by(dataset_type, hb_name, app_quarter_ending, attendance) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

first_att_sco <- first_att_hb |> 
  group_by(dataset_type, app_quarter_ending, attendance) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

first_att_both <- rbind(first_att_hb, first_att_sco) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first"))

rm(first_att_hb, first_att_sco)

#### SEX - All app attendance by sex #####

dna_sex_hb <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended"
  ),
  Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) |>
  group_by(dataset_type, hb_name, app_quarter_ending, attendance, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

dna_sex_sco <- dna_sex_hb |> 
  group_by(dataset_type, app_quarter_ending, attendance, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

dna_sex_both <- rbind(dna_sex_hb, dna_sex_sco) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_sex"))


rm(dna_sex_hb, dna_sex_sco)

#### SEX - First contact  app attendance by sex #####

first_sex_hb <- df_app |>
  filter(att_cat == 1) |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "2" ~ "Clinic cancelled",
    att_status == "3" ~ "Patient cancelled",
    att_status == "5" ~ "Patient CNW",
    att_status == "8" ~ "Patient DNA",
    att_status == "9" ~ "Patient died",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not recorded"
  ),
  Sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) |>
  group_by(dataset_type, hb_name, app_quarter_ending, attendance, Sex) |>  
  summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total

first_sex_sco <- first_sex_hb |> 
  group_by(dataset_type, app_quarter_ending, attendance, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

first_sex_both <- rbind(first_sex_hb, first_sex_sco) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_sex"))

rm(first_sex_hb, first_sex_sco)



 #### AGE - All app attendance by age group ######
 
 dna_age_hb <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   ),
   age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 dna_age_sco <- dna_age_hb |> 
   group_by(dataset_type, app_quarter_ending, attendance, age_group) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 dna_age_both <- rbind(dna_age_hb, dna_age_sco) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_age"))
 
 
 rm(dna_age_hb, dna_age_sco)

 
 #### AGE - First contact  app attendance by age group ######
 
 first_age_hb <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "2" ~ "Clinic cancelled",
     att_status == "3" ~ "Patient cancelled",
     att_status == "5" ~ "Patient CNW",
     att_status == "8" ~ "Patient DNA",
     att_status == "9" ~ "Patient died",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not recorded"
   ),
   age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 first_age_sco <- first_age_hb |> 
   group_by(dataset_type, app_quarter_ending, attendance, age_group) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
first_age_both <- rbind(first_age_hb, first_age_sco) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_age"))
 
 
 rm(first_age_hb, first_age_sco)
 
 
 
 #### SIMD - All app attendance by SIMD #####
 
 dna_simd_hb <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   )) |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, simd2020_quintile) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 dna_simd_sco <- dna_simd_hb |> 
   group_by(dataset_type, app_quarter_ending, attendance, simd2020_quintile) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 dna_simd_both <- rbind(dna_simd_hb, dna_simd_sco) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending, simd2020_quintile) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_simd"))
 
 
 rm(dna_simd_hb, dna_simd_sco)
 
 
 #### SIMD - First contact  app attendance by SIMD ###### 
 
 first_simd_hb <- df_app |>
   filter(att_cat == 1) |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "2" ~ "Clinic cancelled",
     att_status == "3" ~ "Patient cancelled",
     att_status == "5" ~ "Patient CNW",
     att_status == "8" ~ "Patient DNA",
     att_status == "9" ~ "Patient died",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not recorded"
   )) |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, simd2020_quintile) |>  
   summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total
 
 first_simd_sco <- first_simd_hb |> 
   group_by(dataset_type, app_quarter_ending, attendance, simd2020_quintile) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 first_simd_both <- rbind(first_simd_hb, first_simd_sco) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%")) |>
   arrange(dataset_type, hb_name, app_quarter_ending, simd2020_quintile) |>
 save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_simd"))
 
 rm(first_simd_hb, first_simd_sco)
 

 
 
 #### MONTHLY ########
 
 #### Total attended vs non-att apps #######
 
 dna_hb_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   )) |>
   group_by(dataset_type, hb_name, app_month, attendance) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 dna_sco_m <- dna_hb_m |> 
   group_by(dataset_type, app_month, attendance) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 dna_both_m <- rbind(dna_hb_m, dna_sco_m) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_monthly"))
 
 rm(dna_hb_m, dna_sco_m)
 
 #### First contact  apps only - attendance status ####
 
 first_att_hb_m <- df_app |>
   filter(att_cat == 1) |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "2" ~ "Clinic cancelled",
     att_status == "3" ~ "Patient cancelled",
     att_status == "5" ~ "Patient CNW",
     att_status == "8" ~ "Patient DNA",
     att_status == "9" ~ "Patient died",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not recorded"
   )) |>
   group_by(dataset_type, hb_name, app_month, attendance) |>  
   summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total
 
 first_att_sco_m <- first_att_hb_m |> 
   group_by(dataset_type, app_month, attendance) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 first_att_both_m <- rbind(first_att_hb_m, first_att_sco_m) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_monthly"))
 
 rm(first_att_hb_m, first_att_sco_m)
 
 
 
 #### SEX - All app attendance by sex #####
 
 dna_sex_hb_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"),
   Sex = case_when(
     sex_reported == 1 ~ 'Male',
     sex_reported == 2 ~ 'Female',
     sex_reported == 0 ~ 'Not known',
     sex_reported == 9 ~ 'Not specified', 
     TRUE ~ NA_character_)) |>
   group_by(dataset_type, hb_name, app_month, attendance, Sex) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 dna_sex_sco_m <- dna_sex_hb_m |> 
   group_by(dataset_type, app_month, attendance, Sex) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE), .groups = 'drop') |> 
   mutate(hb_name = "NHS Scotland")
 
 dna_sex_both_m <- rbind(dna_sex_hb_m, dna_sex_sco_m) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, Sex) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_sex_monthly"))
 
 
 rm(dna_sex_hb_m, dna_sex_sco_m)
 
 #### SEX - First contact  app attendance by sex #####
 
 first_sex_hb_m <- df_app |>
   filter(att_cat == 1) |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "2" ~ "Clinic cancelled",
     att_status == "3" ~ "Patient cancelled",
     att_status == "5" ~ "Patient CNW",
     att_status == "8" ~ "Patient DNA",
     att_status == "9" ~ "Patient died",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not recorded"
   ),
   Sex = case_when(
     sex_reported == 1 ~ 'Male',
     sex_reported == 2 ~ 'Female',
     sex_reported == 0 ~ 'Not known',
     sex_reported == 9 ~ 'Not specified', 
     TRUE ~ NA_character_)) |>
   group_by(dataset_type, hb_name, app_month, attendance, Sex) |>  
   summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total
 
 first_sex_sco_m <- first_sex_hb_m |> 
   group_by(dataset_type, app_month, attendance, Sex) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 first_sex_both_m <- rbind(first_sex_hb_m, first_sex_sco_m) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, Sex) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_sex_monthly"))
 
 rm(first_sex_hb_m, first_sex_sco_m)
 
 
 #### AGE - All app attendance by age ######
 
 dna_age_hb_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   ),
   age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_month, attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 dna_age_sco_m <- dna_age_hb_m |> 
   group_by(dataset_type, app_month, attendance, age_group) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 dna_age_both_m <- rbind(dna_age_hb_m, dna_age_sco_m) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_age_monthly"))
 
 
 rm(dna_age_hb_m, dna_age_sco_m)
 
 
 #### AGE - First contact  app attendance by age group ######
 
 first_age_hb_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "2" ~ "Clinic cancelled",
     att_status == "3" ~ "Patient cancelled",
     att_status == "5" ~ "Patient CNW",
     att_status == "8" ~ "Patient DNA",
     att_status == "9" ~ "Patient died",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not recorded"
   ),
   age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_month, attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 first_age_sco_m <- first_age_hb_m |> 
   group_by(dataset_type, app_month, attendance, age_group) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 first_age_both_m <- rbind(first_age_hb_m, first_age_sco_m) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_age_monthly"))
 
 
 rm(first_age_hb_m, first_age_sco_m)
 
 
 #### SIMD - All app attendance by SIMD #####
 
 dna_simd_hb_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   )) |>
   group_by(dataset_type, hb_name, app_month, attendance, simd2020_quintile) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 dna_simd_sco_m <- dna_simd_hb_m |> 
   group_by(dataset_type, app_month, attendance, simd2020_quintile) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 dna_simd_both_m <- rbind(dna_simd_hb_m, dna_simd_sco_m) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_simd_monthly"))
 
 
 rm(dna_simd_hb_m, dna_simd_sco_m)
 
 #### SIMD - First contact  app attendance by SIMD ###### 
 
 first_simd_hb_m <- df_app |>
   filter(att_cat == 1) |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "2" ~ "Clinic cancelled",
     att_status == "3" ~ "Patient cancelled",
     att_status == "5" ~ "Patient CNW",
     att_status == "8" ~ "Patient DNA",
     att_status == "9" ~ "Patient died",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not recorded"
   )) |>
   group_by(dataset_type, hb_name, app_month, attendance, simd2020_quintile) |>  
   summarise(appointments = n(), .groups = 'drop') # then sum up apps per day to get total
 
 first_simd_sco_m <- first_simd_hb_m |> 
   group_by(dataset_type, app_month, attendance, simd2020_quintile) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 first_simd_both_m <- rbind(first_simd_hb_m, first_simd_sco_m) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%")) |>
   arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_simd_monthly"))
 
 rm(first_simd_hb_m, first_simd_sco_m)
 
 
 
 
 ###### CREATE EXCELs DOC ########
 # All apps
 # Create a blank workbook
 apps_att <- createWorkbook()
 
 # Add some sheets to the workbook
 addWorksheet(apps_att, "All Apps - All Time")
 addWorksheet(apps_att, "All Apps - Quarterly")
 addWorksheet(apps_att, "All Apps - Monthly")

 addWorksheet(apps_att, "All Apps - All by Sex")
 addWorksheet(apps_att, "All Apps - Qt by Sex")
 addWorksheet(apps_att, "All Apps - Mth by Sex")

 addWorksheet(apps_att, "All Apps - All by Age")
 addWorksheet(apps_att, "All Apps - Qt by Age")
 addWorksheet(apps_att, "All Apps - Mth by Age")
 
 addWorksheet(apps_att, "All Apps - All by SIMD") 
 addWorksheet(apps_att, "All Apps - Qt by SIMD")
 addWorksheet(apps_att, "All Apps - Mth by SIMD")
 #addWorksheet(apps_att, "Sheet 2 Name")
 
 # Write the data to the sheets
 writeData(apps_att, sheet = "All Apps - All Time", x = dna_both_all)
 writeData(apps_att, sheet = "All Apps - Quarterly", x = dna_both)
 writeData(apps_att, sheet = "All Apps - Monthly", x = dna_both_m)

 writeData(apps_att, sheet = "All Apps - All by Sex", x = dna_both_sex_all)
 writeData(apps_att, sheet = "All Apps - Qt by Sex", x = dna_sex_both)
 writeData(apps_att, sheet = "All Apps - Mth by Sex", x = dna_sex_both_m)
 
 writeData(apps_att, sheet = "All Apps - All by Age", x = dna_age_both_all)
 writeData(apps_att, sheet = "All Apps - Qt by Age", x = dna_age_both)
 writeData(apps_att, sheet = "All Apps - Mth by Age", x = dna_age_both_m)
 
 writeData(apps_att, sheet = "All Apps - All by SIMD", x = dna_simd_both_all)
 writeData(apps_att, sheet = "All Apps - Qt by SIMD", x = dna_simd_both)
 writeData(apps_att, sheet = "All Apps - Mth by SIMD", x = dna_simd_both_m)

 # Export the file
 saveWorkbook(apps_att, paste0(shorewise_pub_dir, "/measure_summaries/apps_attendance_forpub.xlsx"), overwrite = TRUE)
 
 
 
 # First contact apps
 apps_first <- createWorkbook()
 
  # Add some sheets to the workbook
 addWorksheet(apps_first, "First Apps - All Time")
 addWorksheet(apps_first, "First Apps - Quarterly")
 addWorksheet(apps_first, "First Apps - Monthly")

 addWorksheet(apps_first, "First Apps - All by Sex")
 addWorksheet(apps_first, "First Apps - Qt by Sex")
 addWorksheet(apps_first, "First Apps - Mth by Sex")
 
 addWorksheet(apps_first, "First Apps - All by Age")
 addWorksheet(apps_first, "First Apps - Qt by Age")
 addWorksheet(apps_first, "First Apps - Mth by Age")
 
 addWorksheet(apps_first, "First Apps - All by SIMD")
 addWorksheet(apps_first, "First Apps - Qt by SIMD")
 addWorksheet(apps_first, "First Apps - Mth by SIMD")

 
 # Write the data to the sheets
 writeData(apps_first, sheet = "First Apps - All Time", x = first_att_both_all)
 writeData(apps_first, sheet = "First Apps - Quarterly", x = first_att_both)
 writeData(apps_first, sheet = "First Apps - Monthly", x = first_att_both_m)
 
 writeData(apps_first, sheet = "First Apps - All by Sex", x = first_sex_both_all)
 writeData(apps_first, sheet = "First Apps - Qt by Sex", x = first_sex_both)
 writeData(apps_first, sheet = "First Apps - Mth by Sex", x = first_sex_both_m)
 
 writeData(apps_first, sheet = "First Apps - All by Age", x = first_age_both_all)
 writeData(apps_first, sheet = "First Apps - Qt by Age", x = first_age_both)
 writeData(apps_first, sheet = "First Apps - Mth by Age", x = first_age_both_m)
 
 writeData(apps_first, sheet = "First Apps - All by SIMD", x = first_simd_both_all)
 writeData(apps_first, sheet = "First Apps - Qt by SIMD", x = first_simd_both)
 writeData(apps_first, sheet = "First Apps - Mth by SIMD", x = first_simd_both_m)
 
 # Export the file
 saveWorkbook(apps_first, paste0(shorewise_pub_dir, "/measure_summaries/first_contact_att_forpub.xlsx"), overwrite = TRUE)
 