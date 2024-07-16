##########################################.
#### APP ATTENDANCE - for publication ####.
##########################################.

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


#### SETUP #####
# set DS choice

#dataset_choice <- "CAMHS"

# dealth with in set constants
# most_recent_month_in_data <- get_lastest_month_end(df_captnd)
# 
# month_end <- floor_date(most_recent_month_in_data, unit = "month")
# month_start <- ymd(month_end) - months(14)
# date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# load data 
df_captnd <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

source("./07_publication/investigations/get_appointments_df.R")

df_app <- get_appointments_df(df_captnd)

# get first appointment date per pathway only
df_first_app <- df_app |>
  group_by(ucpn, patient_id, dataset_type, hb_name) |> 
  arrange(ucpn, app_date) |> 
  slice(1) |> 
  filter(app_month %in% date_range) |> 
  mutate(Attendance = case_when(
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
    TRUE ~ NA_character_)) 

df_app <- df_app |> 
  filter(app_month %in% date_range) 


####### ALL TIME ##########

dna_both_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended"
  )) |>
  group_by(dataset_type, hb_name, attendance) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, attendance) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_alltime"))


#### First contact apps only - attendance status ####

first_att_both_all <- df_first_app |>
  group_by(dataset_type, hb_name, Attendance) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, attendance) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = Attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_alltime"))


#### SEX - All apps attendance all time ####

dna_both_sex_all <- df_app |>
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
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, attendance, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_sex_alltime"))


#### SEX - First contact apps attendance all time ####

first_sex_both_all <- df_first_app |>
  select(-sex_reported) |>
  group_by(dataset_type, hb_name, Attendance, Sex) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, Attendance, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = Attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_sex_alltime"))


#### AGE - All apps attendance all time ####
dna_age_both_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended")) |>
  group_by(dataset_type, hb_name, attendance, age_group) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, attendance, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, (readr::parse_number(age_group))) |> # orders age range) 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_age_alltime"))


#### AGE - First contact apps attendance all time ####
first_age_both_all <- df_first_app |>
  group_by(dataset_type, hb_name, Attendance, age_group) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, Attendance, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = Attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, (readr::parse_number(age_group))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_age_alltime"))


#### SIMD - All apps attendance all time ####
dna_simd_both_all <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended")) |>
  group_by(dataset_type, hb_name, attendance, simd2020_quintile) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, attendance, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, simd2020_quintile) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_simd_alltime"))


#### SIMD - First contact apps attendance all time ####
first_simd_both_all <- df_first_app |>
  group_by(dataset_type, hb_name, Attendance, simd2020_quintile) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, Attendance, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = Attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, simd2020_quintile) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_simd_alltime"))



####### QUARTERLY ##########

#### Total attended vs non-att apps #######

dna_both <- df_app |>
  mutate(attendance = case_when(
    att_status == "1" ~ "Attended",
    att_status == "99" ~ "Not known",
    TRUE ~ "Not Attended"
  )) |>
  group_by(dataset_type, hb_name, app_quarter_ending, attendance) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, attendance) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all"))


#### First contact apps only - attendance status ####

first_att_both <- df_first_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending, Attendance) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, Attendance) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = Attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first"))


#### SEX - All app attendance by sex #####

dna_sex_both <- df_app |>
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
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, attendance, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_sex"))


#### SEX - First contact  app attendance by sex #####

first_sex_both <- df_first_app |>
  group_by(dataset_type, hb_name, app_quarter_ending, Attendance, Sex) |>  
  summarise(appointments = n(), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, Attendance, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = Attendance, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         perc_attended = round(Attended/Total*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_sex"))


 #### AGE - All app attendance by age group ######
 
dna_age_both <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   ),
   age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_quarter_ending, attendance, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_age"))

 
 #### AGE - First contact  app attendance by age group ######
 
first_age_both <- df_first_app |>
   mutate(age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_quarter_ending, Attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_quarter_ending, Attendance, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_age"))
 
 
 #### SIMD - All app attendance by SIMD #####
 
dna_simd_both <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   )) |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, simd2020_quintile) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_quarter_ending, attendance, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending, simd2020_quintile) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_simd"))

 
 #### SIMD - First contact  app attendance by SIMD ###### 
 
first_simd_both <- df_first_app |>
   group_by(dataset_type, hb_name, app_quarter_ending, attendance, simd2020_quintile) |>  
   summarise(appointments = n(), .groups = 'drop') |> 
   group_by(dataset_type, app_quarter_ending, attendance, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%")) |>
   arrange(dataset_type, hb_name, app_quarter_ending, simd2020_quintile) |>
 save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_simd"))
 
 
 
 #### MONTHLY ########
 
 #### Total attended vs non-att apps #######
 
dna_both_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   )) |>
   group_by(dataset_type, hb_name, app_month, attendance) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_month, attendance) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_monthly"))
 
 
 #### First contact  apps only - attendance status ####
 
first_att_both_m <- df_first_app |>
   group_by(dataset_type, hb_name, app_month, Attendance) |>  
   summarise(appointments = n(), .groups = 'drop') |> 
   group_by(dataset_type, app_month, Attendance) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = Attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_monthly"))
 
 
 #### SEX - All app attendance by sex #####
 
dna_sex_both_m <- df_app |>
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
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_month, attendance, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, Sex) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_sex_monthly"))
 
 
 #### SEX - First contact  app attendance by sex #####
 
first_sex_both_m <- df_first_app |>
   group_by(dataset_type, hb_name, app_month, Attendance, Sex) |>  
   summarise(appointments = n(), .groups = 'drop') |> 
   group_by(dataset_type, app_month, Attendance, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = Attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, Sex) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_sex_monthly"))
 
 
 #### AGE - All app attendance by age ######
 
dna_age_both_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   ),
   age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_month, attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_month, attendance, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_age_monthly"))
 
 
 #### AGE - First contact  app attendance by age group ######
 
first_age_both_m <- df_first_app |>
   mutate(age_group = as.character(age_group)) |>
   group_by(dataset_type, hb_name, app_month, Attendance, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_month, Attendance, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   arrange(readr::parse_number(age_group)) |> # orders age range
   pivot_wider(names_from = age_group, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, attendance) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_age_monthly"))

 
 #### SIMD - All app attendance by SIMD #####
 
dna_simd_both_m <- df_app |>
   mutate(attendance = case_when(
     att_status == "1" ~ "Attended",
     att_status == "99" ~ "Not known",
     TRUE ~ "Not Attended"
   )) |>
   group_by(dataset_type, hb_name, app_month, attendance, simd2020_quintile) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   group_by(dataset_type, app_month, attendance, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(Attended:Total, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_all_simd_monthly"))
 
 
 #### SIMD - First contact  app attendance by SIMD ###### 
 
first_simd_both_m <- df_first_app |>
   group_by(dataset_type, hb_name, app_month, Attendance, simd2020_quintile) |>  
   summarise(appointments = n(), .groups = 'drop') |> 
   group_by(dataset_type, app_month, Attendance, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
   pivot_wider(names_from = Attendance, values_from = appointments) |>
   adorn_totals("col") |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          perc_attended = round(Attended/Total*100, 2),
          perc_attended = paste0(perc_attended, "%")) |>
   arrange(dataset_type, hb_name, app_month, simd2020_quintile) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_attendance_first_simd_monthly"))
 
 

# DNA rate only - quarterly by hb - no longer needed
# first_att_dna_qt <- first_att_qt |> 
#   filter(Attendance == "Patient DNA") |> 
#   mutate(app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
#          app_quarter_ending = format(app_quarter_ending, format = "%b '%y"),
#          Percent = round(n/first_contact*100, 1),
#          Percent = paste0(Percent, "%")) |> 
#   select(-c("Attendance", "n", "first_contact")) |> 
#   pivot_wider(names_from = app_quarter_ending, values_from = "Percent") #|> 
#filter(dataset_type = dataset_choice) |> 
#save_as_parquet(paste0(shorewise_pub_data_dir, "/", measure_label, "qt_dna_rate_hb")) # _", dataset_choice))
 
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
 