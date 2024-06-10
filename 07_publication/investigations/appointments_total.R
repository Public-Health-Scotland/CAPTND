##############################################.
#### TOTAL APPOINTMENTS - for publication ####.
##############################################.


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')

#### setup ######

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

# select vars to bind into data
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


df_attend <- df_app |>
  filter(att_status == "1")


###### ALL TIME #######

# get appointments

df_hb_all <- df_app |> 
  group_by(dataset_type, hb_name) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

df_sco_all <- df_hb_all |> 
  group_by(dataset_type) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

df_all_app_all <- bind_rows(df_hb_all, df_sco_all) #|>
#filter(dataset_type == dataset_choice) 


# get attended appointments

df_hb_att_all <- df_attend |> 
  group_by(dataset_type, hb_name) |>  
  summarise(attended = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

df_sco_att_all <- df_hb_att_all |> 
  group_by(dataset_type) |> 
  summarise(attended = sum(attended, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

df_all_att_all <- bind_rows(df_hb_att_all, df_sco_att_all) 

# appended attended to total appointments

df_all_all <- inner_join(df_all_app_all, df_all_att_all, 
                     by = c("dataset_type", "hb_name")) |>
  mutate(perc_attended = round(attended/appointments*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         hb_name = factor(hb_name, levels = level_order_hb),
         across(appointments:attended, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  #filter(dataset_type == dataset_choice) 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_att_alltime")) #_", dataset_choice

# make nice tables of apps alone

df_apps_all <- df_all_all |> 
  #filter(dataset_type == dataset_choice) |> 
  select(- c(attended, perc_attended)) |>
  arrange(dataset_type, hb_name) |> 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_alltime")) #_", dataset_choice

rm(df_hb_att_all, df_sco_att_all, df_all_att_all, df_hb_all, df_sco_all, df_all_app_all)


#### Apps by SEX ########
df_sex_hb <- df_app |>
  select(all_of(data_keys), app_date, app_month, sex_reported, n_app_patient_same_day) |>  
  mutate(sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) |>
  select(-sex_reported) |>
  group_by(dataset_type, hb_name, sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 


df_sex_sco <- df_sex_hb |>
  group_by(dataset_type, sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")


df_sex <- rbind(df_sex_hb, df_sex_sco) |>
  pivot_wider(names_from = sex, values_from = appointments) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         across(Female:`NA`, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_sex_alltime")) #_", dataset_choice

rm(df_sex_hb, df_sex_sco)


#### Apps by AGE ########

df_age_hb <- df_app |>
  select(all_of(data_keys), app_date, app_month, age_at_ref_rec, age_group, n_app_patient_same_day) |>  
  group_by(dataset_type, hb_name, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 

df_age_sco <- df_age_hb |>
  group_by(dataset_type, age_group) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")


df_age <- rbind(df_age_hb, df_age_sco) |>
  arrange(readr::parse_number(age_group)) |> # orders age range 
  pivot_wider(names_from = age_group, values_from = appointments) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         across(`0-4`:`NA`, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_age_alltime")) #_", dataset_choice

rm(df_age_hb, df_age_sco)

#### Apps by SIMD ########

df_simd_hb_all <- df_app |>
  select(all_of(data_keys), app_date, app_month, simd2020_quintile, n_app_patient_same_day) |>  
  group_by(dataset_type, hb_name, simd2020_quintile) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 

df_simd_sco_all <- df_simd_hb_all |>
  group_by(dataset_type, simd2020_quintile) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")


df_simd_all <- rbind(df_simd_hb_all, df_simd_sco_all) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         across(`1`:`NA`, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_simd_alltime")) #_", dataset_choice

rm(df_simd_hb_all, df_simd_sco_all)



###### QUARTERLY #######


# get quarterly appointments

df_hb <- df_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

df_sco <- df_hb |> 
  group_by(dataset_type, app_quarter_ending) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

df_all_app <- bind_rows(df_hb, df_sco) #|>
  #filter(dataset_type == dataset_choice) 


# get quarterly attended appointments

df_hb_att <- df_attend |> 
  group_by(dataset_type, hb_name, app_quarter_ending) |>  
  summarise(attended = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

df_sco_att <- df_hb_att |> 
  group_by(dataset_type, app_quarter_ending) |> 
  summarise(attended = sum(attended, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

df_all_att <- bind_rows(df_hb_att, df_sco_att) 


# appended attended to total appointments

df_all <- inner_join(df_all_app, df_all_att, 
                     by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
  mutate(perc_attended = round(attended/appointments*100, 2),
         perc_attended = paste0(perc_attended, "%"),
         across(appointments:attended, ~prettyNum(., big.mark = ","))) |>
  #filter(dataset_type == dataset_choice) 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_att_quarterly")) #_", dataset_choice

rm(df_hb_att, df_sco_att, df_hb, df_sco, df_all_app, df_all_att)


# make nice tables of apps quarterly alone

df_apps_quarterly <- df_all |> 
  #filter(dataset_type == dataset_choice) |> 
  select(- c(attended, perc_attended)) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> 
  arrange(hb_name) |> 
  mutate(app_quarter_ending = format(app_quarter_ending, "%b %Y")) |> 
  pivot_wider(names_from = app_quarter_ending, values_from = appointments) |> 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_quarterly")) #_", dataset_choice


#### Apps by SEX ########

df_sex_hb_q <- df_app |>
  select(all_of(data_keys), app_date, app_quarter_ending, sex_reported, n_app_patient_same_day) |>  
  mutate(sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    sex_reported == 0 ~ 'Not known',
    sex_reported == 9 ~ 'Not specified', 
    TRUE ~ NA_character_)) |>
  select(-sex_reported) |>
  group_by(dataset_type, hb_name, app_quarter_ending, sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 


df_sex_sco_q <- df_sex_hb_q |>
  group_by(dataset_type, app_quarter_ending, sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")


df_sex_q <- rbind(df_sex_hb_q, df_sex_sco_q) |>
  pivot_wider(names_from = sex, values_from = appointments) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m_%d"),
         across(Female:`NA`, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_sex_quarterly")) #_", dataset_choice

rm(df_sex_hb_q, df_sex_sco_q)


 #### Apps by AGE ########
 
 df_age_hb_q <- df_app |>
   select(all_of(data_keys), app_date, app_quarter_ending, age_group, n_app_patient_same_day) |>  
   group_by(dataset_type, hb_name, app_quarter_ending, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 
 
 df_age_sco_q <- df_age_hb_q |>
   group_by(dataset_type, app_quarter_ending, age_group) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 
 df_age_q <- rbind(df_age_hb_q, df_age_sco_q) |>
   arrange(readr::parse_number(age_group)) |> # orders age range 
   pivot_wider(names_from = age_group, values_from = appointments) |>
   #filter(dataset_type == dataset_choice) |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m_%d"),
          across(`0-4`:`NA`, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_quarter_ending) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_age_quarterly")) #_", dataset_choice
 
rm(df_age_hb_q, df_age_sco_q)


#### Apps by SIMD ##########

df_simd_hb <- df_app |> 
  mutate(simd2020_quintile = as.character(simd2020_quintile)) |> 
  select(all_of(data_keys), ref_acc, app_month, app_date, app_quarter_ending, 
         simd2020_quintile, n_app_patient_same_day) |>
  group_by(dataset_type, app_quarter_ending, simd2020_quintile, hb_name) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop')


df_simd_sco <- df_simd_hb |>   
  group_by(dataset_type, app_quarter_ending, simd2020_quintile) |>  
  summarise(appointments = sum(appointments), .groups = 'drop') |>
  mutate(hb_name = "NHS Scotland") 

df_simd <- rbind(df_simd_hb, df_simd_sco) |>
  #filter(dataset_type == dataset_choice) |> 
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         across(`1`:Total, ~prettyNum(., big.mark = ","))) |> 
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_simd_quarterly")) #_", dataset_choice

rm(df_simd_hb, df_simd_sco)

 

 ###### MONTHLY ######
 
 # monthly apps and attended apps for scotland-wide plots
 
 apps_monthly_sco_all <- df_app |>
   select(all_of(data_keys), ref_acc, app_date, app_month, n_app_patient_same_day) |> 
   group_by(app_month, dataset_type) |> 
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 
 
 
 apps_monthly_sco_att <- df_attend |>
   select(all_of(data_keys), ref_acc, app_date, app_month, n_app_patient_same_day) |> 
   group_by(app_month, dataset_type) |> 
   summarise(attended = sum(n_app_patient_same_day), .groups = 'drop') 
 
 apps_monthly_sco <- inner_join(apps_monthly_sco_all, apps_monthly_sco_att, 
                                by = c("dataset_type", "app_month")) |>
   mutate(perc_attended = round(attended/appointments*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          across(appointments:attended, ~prettyNum(., big.mark = ","))) |>
   #filter(dataset_type == dataset_choice) |> 
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_att_monthly_sco")) #_", dataset_choice
 
 rm(apps_monthly_sco_att, apps_monthly_sco_all)
 
 
 # get monthly appointments
 
 df_hb_m <- df_app |> 
   group_by(dataset_type, hb_name, app_month) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 df_sco_m <- df_hb_m |> 
   group_by(dataset_type, app_month) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 df_all_app_m <- bind_rows(df_hb_m, df_sco_m) #|>
 #filter(dataset_type == dataset_choice) 
 
 
 # get quarterly attended appointments
 
 df_hb_att_m <- df_attend |> 
   group_by(dataset_type, hb_name, app_month) |>  
   summarise(attended = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total
 
 df_sco_att_m <- df_hb_att_m |> 
   group_by(dataset_type, app_month) |> 
   summarise(attended = sum(attended, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 df_all_att_m <- bind_rows(df_hb_att_m, df_sco_att_m) 

 
 # appended attended to total appointments
 
 df_all_m <- inner_join(df_all_app_m, df_all_att_m, 
                      by = c("dataset_type", "hb_name", "app_month")) |>
   mutate(perc_attended = round(attended/appointments*100, 2),
          perc_attended = paste0(perc_attended, "%"),
          hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          across(appointments:attended, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   #filter(dataset_type == dataset_choice) 
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_att_monthly")) #_", dataset_choice
 
 
 rm(df_hb_att_m, df_sco_att_m, df_hb_m, df_sco_m, df_all_app_m, df_all_att_m)
 
 # make nice tables of apps monthly alone
 
 df_apps_monthly <- df_all_m |> 
   #filter(dataset_type == dataset_choice) |> 
   select(- c(attended, perc_attended)) |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> 
   arrange(dataset_type, hb_name, app_month) |> 
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_monthly")) #_", dataset_choice
 

 #### Apps by SEX ########
 
 df_sex_hb_m <- df_app |>
   select(all_of(data_keys), app_date, app_month, sex_reported, n_app_patient_same_day) |>  
   mutate(sex = case_when(
     sex_reported == 1 ~ 'Male',
     sex_reported == 2 ~ 'Female',
     sex_reported == 0 ~ 'Not known',
     sex_reported == 9 ~ 'Not specified', 
     TRUE ~ NA_character_)) |>
   select(-sex_reported) |>
   group_by(dataset_type, hb_name, app_month, sex) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 
 
 
 df_sex_sco_m <- df_sex_hb_m |>
   group_by(dataset_type, app_month, sex) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 
 df_sex_m <- rbind(df_sex_hb_m, df_sex_sco_m) |>
   pivot_wider(names_from = sex, values_from = appointments) |>
   #filter(dataset_type == dataset_choice) |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m_%d"),
          across(Female:`NA`, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_sex_monthly")) #_", dataset_choice
 
 rm(df_sex_hb_m, df_sex_sco_m)
 
 
 #### Apps by AGE ########
 
 df_age_hb_m <- df_app |>
   select(all_of(data_keys), app_date, app_month, age_group, n_app_patient_same_day) |>  
   group_by(dataset_type, hb_name, app_month, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') 
 
 df_age_sco_m <- df_age_hb_m |>
   group_by(dataset_type, app_month, age_group) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland")
 
 
 df_age_m <- rbind(df_age_hb_m, df_age_sco_m) |>
   arrange(readr::parse_number(age_group)) |> # orders age range 
   pivot_wider(names_from = age_group, values_from = appointments) |>
   #filter(dataset_type == dataset_choice) |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m_%d"),
          across(`0-4`:`NA`, ~prettyNum(., big.mark = ","))) |>
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_age_monthly")) #_", dataset_choice
 
 rm(df_age_hb_m, df_age_sco_m)
 
 
#### Apps by SIMD #####
 
 df_simd_hb_m <- df_app |> 
   mutate(simd2020_quintile = as.character(simd2020_quintile)) |> 
   select(all_of(data_keys), ref_acc, app_month,  
          simd2020_quintile, n_app_patient_same_day) |>
   group_by(dataset_type, hb_name, app_month, simd2020_quintile) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop')
 
 
 df_simd_sco_m <- df_simd_hb_m |>
   group_by(dataset_type, app_month, simd2020_quintile) |> 
   summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
   mutate(hb_name = "NHS Scotland") 
 
 df_simd_m <- rbind(df_simd_hb_m, df_simd_sco_m) |>
   #filter(dataset_type == dataset_choice) |> 
   pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb),
          app_month = as.Date(app_month, "%Y-%m-%d"),
          across(`1`:`NA`, ~prettyNum(., big.mark = ","))) |> 
   arrange(dataset_type, hb_name, app_month) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_simd_monthly")) #_", dataset_choice
 
 rm(df_simd_hb_m, df_simd_sco_m)
 
 
 

 
 ###### CREATE EXCEL DOC ########
 
 # Create a blank workbook
 apps_tot <- createWorkbook()
 
 # Add some sheets to the workbook
 addWorksheet(apps_tot, "Att Apps - All Time")
 addWorksheet(apps_tot, "Att Apps - Quarterly")
 addWorksheet(apps_tot, "Att Apps - Monthly")
 addWorksheet(apps_tot, "Att Apps - Monthly Scotland")
  
 addWorksheet(apps_tot, "Total Apps - All Time")
 addWorksheet(apps_tot, "Total Apps - Quarterly")
 addWorksheet(apps_tot, "Total Apps - Monthly")
 
 addWorksheet(apps_tot, "Total Apps - All by Sex")
 addWorksheet(apps_tot, "Total Apps - Qt by Sex")
 addWorksheet(apps_tot, "Total Apps - Mth by Sex")
 
 addWorksheet(apps_tot, "Total Apps - All by Age")
 addWorksheet(apps_tot, "Total Apps - Qt by Age")
 addWorksheet(apps_tot, "Total Apps - Mth by Age")
 
 addWorksheet(apps_tot, "Total Apps - All by SIMD")
 addWorksheet(apps_tot, "Total Apps - Qt by SIMD")
 addWorksheet(apps_tot, "Total Apps - Mth by SIMD")
 
 #addWorksheet(apps_tot, "Sheet 2 Name")
 
 # Write the data to the sheets
 writeData(apps_tot, sheet = "Att Apps - All Time", x = df_all_all)
 writeData(apps_tot, sheet = "Att Apps - Quarterly", x = df_all)
 writeData(apps_tot, sheet = "Att Apps - Monthly", x = df_all_m)
 writeData(apps_tot, sheet = "Att Apps - Monthly Scotland", x = apps_monthly_sco)
 
 writeData(apps_tot, sheet = "Total Apps - All Time", x = df_apps_all)
 writeData(apps_tot, sheet = "Total Apps - Quarterly", x = df_apps_quarterly)
 writeData(apps_tot, sheet = "Total Apps - Monthly", x = df_apps_monthly)
 
 writeData(apps_tot, sheet = "Total Apps - All by Sex", x = df_sex)
 writeData(apps_tot, sheet = "Total Apps - Qt by Sex", x = df_sex_q)
 writeData(apps_tot, sheet = "Total Apps - Mth by Sex", x = df_sex_m)
 
 writeData(apps_tot, sheet = "Total Apps - All by Age", x = df_age)
 writeData(apps_tot, sheet = "Total Apps - Qt by Age", x = df_age_q)
 writeData(apps_tot, sheet = "Total Apps - Mth by Age", x = df_age_m)

 writeData(apps_tot, sheet = "Total Apps - All by SIMD", x = df_simd_all) 
 writeData(apps_tot, sheet = "Total Apps - Qt by SIMD", x = df_simd)
 writeData(apps_tot, sheet = "Total Apps - Mth by SIMD", x = df_simd_m)
 

 # Export the file
 saveWorkbook(apps_tot, paste0(shorewise_pub_dir, "/measure_summaries/apps_total_forpub.xlsx"), overwrite = TRUE)
 
 