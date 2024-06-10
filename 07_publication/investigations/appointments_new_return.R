###############################################.
#### NEW AND RETURN APPS - for publication ####.
###############################################.

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



###### ALL TIME ######
#### Total new vs return apps #######

nr_hb_all <- df_app |> 
  group_by(dataset_type, hb_name, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_sco_all <- nr_hb_all |> 
  group_by(dataset_type, att_cat) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_both_all <- bind_rows(nr_hb_all, nr_sco_all) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         hb_name = factor(hb_name, levels = level_order_hb)) |> 
  pivot_wider(names_from = att_cat, values_from = appointments) |> 
  adorn_totals("col") |>
  mutate(across(New:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_alltime")) #_", dataset_choice

rm(nr_hb_all, nr_sco_all) 

#### SEX - all time ####
nr_sex_hb_all <- df_app |> 
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |> 
  group_by(dataset_type, hb_name, att_cat, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_sex_sco_all <- nr_sex_hb_all |> 
  group_by(dataset_type, att_cat, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_sex_both_all <- bind_rows(nr_sex_hb_all, nr_sex_sco_all) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
  unnest(cols = everything()) |>
  adorn_totals("col") |>
  mutate(across(New:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_sex_alltime"))

rm(nr_sex_hb_all, nr_sex_sco_all) 


#### AGE - all time ####
nr_age_hb_all <- df_app |> 
  group_by(dataset_type, hb_name, att_cat, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_age_sco_all <- nr_age_hb_all |> 
  group_by(dataset_type, att_cat, age_group) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_age_both_all <- bind_rows(nr_age_hb_all, nr_age_sco_all) |>
  # filter(dataset_type = dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |>
  arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = appointments) |>
  arrange(dataset_type, hb_name, att_cat) |>
  adorn_totals("col") |>
  mutate(across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_age_alltime"))

rm(nr_age_hb_all, nr_age_sco_all) 


#### SIMD - all time ####

nr_simd_hb_all <- df_app |>
  group_by(dataset_type, hb_name, att_cat, simd2020_quintile) |>
  summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") 

nr_simd_sco_all <- nr_simd_hb_all |>
  group_by(dataset_type,  att_cat, simd2020_quintile) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_simd_both_all <- rbind(nr_simd_hb_all, nr_simd_sco_all) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  arrange(dataset_type, hb_name, att_cat) |>
  adorn_totals("col") |>
  mutate(across(`1`:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_simd_alltime"))

rm(nr_simd_hb_all, nr_simd_sco_all)



####### QUARTERLY #######
#### Total new vs return apps #######

nr_hb <- df_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_sco <- nr_hb |> 
  group_by(dataset_type, app_quarter_ending, att_cat) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_all <- bind_rows(nr_hb, nr_sco) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  pivot_wider(names_from = att_cat, values_from = appointments) |> 
  adorn_totals("col") |>
  mutate(app_quarter_ending = format(app_quarter_ending, "%b %Y"), 
         across(New:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_quarterly")) #_", dataset_choice

rm(nr_hb, nr_sco)  



#### SEX - New vs Return Apps: by sex ######

nr_sex_hb <- df_app |> 
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_sex_sco <- nr_sex_hb |> 
  group_by(dataset_type, app_quarter_ending, att_cat, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_sex_all <- bind_rows(nr_sex_hb, nr_sex_sco) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
  unnest(cols = everything()) |>
  adorn_totals("col") |>
  mutate(app_quarter_ending = as.Date(app_quarter_ending, format = "%Y-%m-%d"),
         across(New:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_sex"))

rm(nr_sex_hb, nr_sex_sco)  


#### AGE - New vs Return Apps: by age ######

nr_age_hb <- df_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_age_sco <- nr_age_hb |> 
  group_by(dataset_type, app_quarter_ending, att_cat, age_group) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_age_all <- bind_rows(nr_age_hb, nr_age_sco) |>
  # filter(dataset_type = dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         app_quarter_ending = as.Date(app_quarter_ending, format = "%Y-%m-%d")) |>
  arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = appointments) |>
  arrange(dataset_type, hb_name, app_quarter_ending, att_cat) |>
  adorn_totals("col") |>
  mutate(across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_age"))

rm(nr_age_hb, nr_age_sco)  


#### SIMD - New vs Return Apps: by SIMD ######

nr_simd_hb <- df_app |>
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat, simd2020_quintile) |>
  summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") 

nr_simd_sco <- nr_simd_hb |>
  group_by(dataset_type, app_quarter_ending, att_cat, simd2020_quintile) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_simd_both <- rbind(nr_simd_hb, nr_simd_sco) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  arrange(dataset_type, hb_name, app_quarter_ending, att_cat) |>
  adorn_totals("col") |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_simd"))

rm(nr_simd_hb, nr_simd_sco)


####### MONTHLY #######
#### Total new vs return apps #######

nr_hb_m <- df_app |> 
  group_by(dataset_type, hb_name, app_month, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_sco_m <- nr_hb_m |> 
  group_by(dataset_type, app_month, att_cat) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_all_m <- bind_rows(nr_hb_m, nr_sco_m) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  pivot_wider(names_from = att_cat, values_from = appointments) |> 
  adorn_totals("col") |>
  mutate(app_month = as.Date(app_month, "%Y-&m-&d"), 
         across(New:Total, ~prettyNum(., big.mark = ",")),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(dataset_type, hb_name, app_month) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_monthly")) #_", dataset_choice

rm(nr_hb_m, nr_sco_m) 


#### SEX - New vs Return Apps: by sex ######

nr_sex_hb_m <- df_app |> 
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |> 
  group_by(dataset_type, hb_name, app_month, att_cat, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_sex_sco_m <- nr_sex_hb_m |> 
  group_by(dataset_type, app_month, att_cat, Sex) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_sex_all_m <- bind_rows(nr_sex_hb_m, nr_sex_sco_m) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
  unnest(cols = everything()) |>
  adorn_totals("col") |>
  mutate(app_month = as.Date(app_month, format = "%Y-%m-%d"),
         across(New:Total, ~prettyNum(., big.mark = ",")),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(dataset_type, hb_name, app_month) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_sex_monthly"))

rm(nr_sex_hb_m, nr_sex_sco_m)  


#### AGE - New vs Return Apps: by age ######

nr_age_hb_m <- df_app |> 
  group_by(dataset_type, hb_name, app_month, att_cat, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

nr_age_sco_m <- nr_age_hb_m |> 
  group_by(dataset_type, app_month, att_cat, age_group) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_age_all_m <- bind_rows(nr_age_hb_m, nr_age_sco_m) |>
  # filter(dataset_type = dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         app_month = as.Date(app_month, format = "%Y-%m-%d"),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = appointments) |>
  arrange(dataset_type, hb_name, app_month, att_cat) |>
  adorn_totals("col") |>
  mutate(across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_age_monthly"))

rm(nr_age_hb_m, nr_age_sco_m)  


#### SIMD - New vs Return Apps: by SIMD ######

nr_simd_hb_m <- df_app |>
  group_by(dataset_type, hb_name, app_month, att_cat, simd2020_quintile) |>
  summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") 

nr_simd_sco_m <- nr_simd_hb_m |>
  group_by(dataset_type, app_month, att_cat, simd2020_quintile) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

nr_simd_both_m <- rbind(nr_simd_hb_m, nr_simd_sco_m) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_month = as.Date(app_month, "%Y-%m-%d"),
         att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  arrange(dataset_type, hb_name, app_month, att_cat) |>
  adorn_totals("col") |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_simd_monthly"))

rm(nr_simd_hb_m, nr_simd_sco_m)




###### CREATE EXCEL DOC ########

# Create a blank workbook
apps_nr <- createWorkbook()

# Add some sheets to the workbook
addWorksheet(apps_nr, "New Return Apps - All Time")
addWorksheet(apps_nr, "New Return Apps - Quarterly")
addWorksheet(apps_nr, "New Return Apps - Monthly")

addWorksheet(apps_nr, "New Return Apps - All by Sex")
addWorksheet(apps_nr, "New Return Apps - Qt by Sex")
addWorksheet(apps_nr, "New Return Apps - Mth by Sex")

addWorksheet(apps_nr, "New Return Apps - All by Age")
addWorksheet(apps_nr, "New Return Apps - Qt by Age")
addWorksheet(apps_nr, "New Return Apps - Mth by Age")

addWorksheet(apps_nr, "New Return Apps - All by SIMD")
addWorksheet(apps_nr, "New Return Apps - Qt by SIMD")
addWorksheet(apps_nr, "New Return Apps - Mth by SIMD")

#addWorksheet(apps_nr, "Sheet 2 Name")

# Write the data to the sheets
writeData(apps_nr, sheet = "New Return Apps - All Time", x = nr_both_all)
writeData(apps_nr, sheet = "New Return Apps - Quarterly", x = nr_all)
writeData(apps_nr, sheet = "New Return Apps - Monthly", x = nr_all_m)

writeData(apps_nr, sheet = "New Return Apps - All by Sex", x = nr_sex_both_all)
writeData(apps_nr, sheet = "New Return Apps - Qt by Sex", x = nr_sex_all)
writeData(apps_nr, sheet = "New Return Apps - Mth by Sex", x = nr_sex_all_m)

writeData(apps_nr, sheet = "New Return Apps - All by Age", x = nr_age_both_all)
writeData(apps_nr, sheet = "New Return Apps - Qt by Age", x = nr_age_all)
writeData(apps_nr, sheet = "New Return Apps - Mth by Age", x = nr_age_all_m)

writeData(apps_nr, sheet = "New Return Apps - All by SIMD", x = nr_simd_both_all)
writeData(apps_nr, sheet = "New Return Apps - Qt by SIMD", x = nr_simd_both)
writeData(apps_nr, sheet = "New Return Apps - Mth by SIMD", x = nr_simd_both_m)


# Export the file
saveWorkbook(apps_nr, paste0(shorewise_pub_dir, "/measure_summaries/apps_new_return_forpub.xlsx"), overwrite = TRUE)
