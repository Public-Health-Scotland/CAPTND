###############################################.
#### NEW AND RETURN APPS - for publication ####.
###############################################.

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


#### SETUP #####
# set DS choice

#dataset_choice <- "CAMHS"


# # dealth with in set constants
# most_recent_month_in_data <- get_lastest_month_end(df_captnd)
# 
# month_end <- floor_date(most_recent_month_in_data, unit = "month")
# month_start <- ymd(month_end) - months(14)
# date_range <- seq.Date(from = month_start, to = month_end, by = "month")

# load data 
df_captnd <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))

source("./07_publication/investigations/get_appointments_df.R")

df_app <- get_appointments_df(df_captnd) |>
  filter(app_month %in% date_range)



###### ALL TIME ######
#### Total new vs return apps #######

nr_both_all <- df_app |> 
  group_by(dataset_type, hb_name, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, att_cat) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |> 
  #filter(dataset_type == dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         hb_name = factor(hb_name, levels = level_order_hb)) |> 
  pivot_wider(names_from = att_cat, values_from = appointments) |> 
  adorn_totals("col") |>
  mutate(across(New:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_alltime")) #_", dataset_choice
 

#### SEX - all time ####
nr_sex_both_all <- df_app |> 
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |> 
  group_by(dataset_type, hb_name, att_cat, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, att_cat, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
  unnest(cols = everything()) |>
  adorn_totals("col") |>
  mutate(across(New:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_sex_alltime"))


#### AGE - all time ####

nr_age_both_all <- df_app |> 
  group_by(dataset_type, hb_name, att_cat, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, att_cat, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
 # filter(dataset_type = dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |>
  arrange(readr::parse_number(age_group)) |> # orders age range
  pivot_wider(names_from = age_group, values_from = appointments) |>
  arrange(dataset_type, hb_name, att_cat) |>
  adorn_totals("col") |>
  mutate(across(`0-4`:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_age_alltime"))


#### SIMD - all time ####

nr_simd_both_all <- df_app |>
  group_by(dataset_type, hb_name, att_cat, simd2020_quintile) |>
  summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") |>
  group_by(dataset_type,  att_cat, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  arrange(dataset_type, hb_name, att_cat) |>
  adorn_totals("col") |>
  mutate(across(`1`:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_simd_alltime"))



####### QUARTERLY #######
#### Total new vs return apps #######

nr_all <- df_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, att_cat) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  pivot_wider(names_from = att_cat, values_from = appointments) |> 
  adorn_totals("col") |>
  mutate(app_quarter_ending = format(app_quarter_ending, "%b %Y"), 
         across(New:Total, ~prettyNum(., big.mark = ","))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_quarterly")) #_", dataset_choice


#### SEX - New vs Return Apps: by sex ######

nr_sex_all <- df_app |> 
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, att_cat, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
  unnest(cols = everything()) |>
  adorn_totals("col") |>
  mutate(app_quarter_ending = as.Date(app_quarter_ending, format = "%Y-%m-%d"),
         across(New:Total, ~prettyNum(., big.mark = ","))) |>
  arrange(dataset_type, hb_name, app_quarter_ending, Sex) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_sex"))


#### AGE - New vs Return Apps: by age ######

nr_age_all <- df_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_quarter_ending, att_cat, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
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


#### SIMD - New vs Return Apps: by SIMD ######

nr_simd_both <- df_app |>
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat, simd2020_quintile) |>
  summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") |>
  group_by(dataset_type, app_quarter_ending, att_cat, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_quarter_ending = as.Date(app_quarter_ending, "%Y-%m-%d"),
         att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         across(1:Total, ~prettyNum(., big.mark = ","))) |> 
  arrange(dataset_type, hb_name, app_quarter_ending, att_cat) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_simd"))



####### MONTHLY #######
#### Total new vs return apps #######

nr_all_m <- df_app |> 
  group_by(dataset_type, hb_name, app_month, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_month, att_cat)  %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
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


#### SEX - New vs Return Apps: by sex ######

nr_sex_all_m <- df_app |> 
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         Sex = case_when(
           sex_reported == 1 ~ 'Male',
           sex_reported == 2 ~ 'Female',
           sex_reported == 0 ~ 'Not known',
           sex_reported == 9 ~ 'Not specified', 
           TRUE ~ NA_character_)) |> 
  group_by(dataset_type, hb_name, app_month, att_cat, Sex) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_month, att_cat, Sex) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = att_cat, values_from = appointments, values_fn = list) |> 
  unnest(cols = everything()) |>
  adorn_totals("col") |>
  mutate(app_month = as.Date(app_month, format = "%Y-%m-%d"),
         across(New:Total, ~prettyNum(., big.mark = ",")),
         hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(dataset_type, hb_name, app_month) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_sex_monthly"))


#### AGE - New vs Return Apps: by age ######

nr_age_all_m <- df_app |> 
  group_by(dataset_type, hb_name, app_month, att_cat, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(dataset_type, app_month, att_cat, age_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
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


#### SIMD - New vs Return Apps: by SIMD ######

nr_simd_both_m <- df_app |>
  group_by(dataset_type, hb_name, app_month, att_cat, simd2020_quintile) |>
  summarise(appointments = sum(n_app_patient_same_day), .groups = "drop") |>
  group_by(dataset_type, app_month, att_cat, simd2020_quintile) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(hb_name, ~"NHS Scotland"),
                      .groups = "drop")) |>
  # filter(dataset_type = dataset_choice) |>
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  adorn_totals("col") |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb),
         app_month = as.Date(app_month, "%Y-%m-%d"),
         att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return"),
         across(1:Total, ~prettyNum(., big.mark = ","))) |> 
  arrange(dataset_type, hb_name, app_month, att_cat) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_simd_monthly"))



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
