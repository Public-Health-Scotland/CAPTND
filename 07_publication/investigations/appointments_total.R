
#### USING PUB REFS FUNCTIONS AS TEMPLATE ######################################

source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


# set DS choice

#dataset_choice <- "CAMHS"


df_captnd <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(app_month = floor_date(app_date, unit = "month"),
         app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(app_quarter, unit = "month")) 

# constants
most_recent_month_in_data <- get_lastest_month_end(df_captnd)

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")

df_captnd <- df_captnd |>
  filter(app_month %in% date_range)

# select vars
df_app <- df_captnd |>
  select(all_of(data_keys), all_of(demographics), ref_acc, app_date, app_month, att_status, 
         att_cat, app_quarter_ending) |> # app_date not sufficient need to account for multiples
  mutate(app_quarter_ending = as.Date(app_quarter_ending)) |>
  filter(!is.na(app_date)) |> 
  group_by(across(all_of(c(data_keys, app_month_o, app_date_o)))) |> 
  summarise(n_app_patient_same_day = n(), # count up apps per day for each pathway
            across(), .groups = 'drop') # 'across()' keeps other columns


df_attend <- df_app |>
  filter(att_status == "1")

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

rm(df_hb, df_sco)

#attended

df_hb_att <- df_attend |> 
  group_by(dataset_type, hb_name, app_quarter_ending) |>  
  summarise(attended = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

df_sco_att <- df_hb_att |> 
  group_by(dataset_type, app_quarter_ending) |> 
  summarise(attended = sum(attended, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

df_all_att <- bind_rows(df_hb_att, df_sco_att) 

rm(df_hb_att, df_sco_att)

# appended attended to df_all

df_all <- inner_join(df_all_app, df_all_att, 
                     by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
  mutate(perc_attended = round(attended/appointments*100, 2),
         perc_attended = paste0(perc_attended, "%")) |>
  #filter(dataset_type == dataset_choice) 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_att_quarterly")) #_", dataset_choice


# monthly apps for scotland-wide plots

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
         perc_attended = paste0(perc_attended, "%")) |>
  #filter(dataset_type == dataset_choice) |> 
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_att_monthly_sco")) #_", dataset_choice

rm(apps_monthly_sco_att, apps_monthly_sco_all)

# make nice tables of apps quarterly


df_apps_quarterly <- df_all |> 
  #filter(dataset_type == dataset_choice) |> 
  select(- c(attended, perc_attended)) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> 
  arrange(hb_name) |> 
  mutate(app_quarter_ending = format(app_quarter_ending, "%b %Y"), 
         appointments = format(appointments, big.mark=",")) |> 
  pivot_wider(names_from = app_quarter_ending, values_from = appointments) |> 
  mutate_all(~replace(., is.na(.), "..")) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_quarterly")) #_", dataset_choice




#### SIMD ######################################################################
## does this need to be population-adjusted for appointments?? because apps not one-per-pathway...

# # load reference data
# data_working_safe <- "../../../output/publication/data/"
# reference_files_dir <- paste0(data_working_safe, 'reference_files/')
# 
# df_pop_simd <- import(paste0(reference_files_dir, 'pop_age_SIMD.xlsx')) |> 
#   pivot_longer(cols = c(2:92), names_to = "Age", values_to = "Count") |> 
#   mutate(Age = case_when(
#     Age == "Age90plus" ~ "Age90",
#     TRUE ~ Age),
#     Age = as.numeric(str_replace(Age, "Age", "")))
# 
# df_pop_simd_camhs <- df_pop_simd |> 
#   filter(Age %in% c(0:18)) |> 
#   group_by(SIMD2020_5) |> 
#   summarise(Count = sum(Count)) |> 
#   mutate(dataset_type = "CAMHS", .before = "SIMD2020_5")
# 
# df_pop_simd_pt <- df_pop_simd |> 
#   group_by(SIMD2020_5) |> 
#   summarise(Count = sum(Count)) |> 
#   mutate(dataset_type = "PT", .before = "SIMD2020_5")
# 
# df_pop_simd <- bind_rows(df_pop_simd_camhs, df_pop_simd_pt) |> 
#   rename(population_size = Count)



df_simd_hb <- df_app |> 
  mutate(simd2020_quintile = as.character(simd2020_quintile)) |> 
  select(all_of(data_keys), ref_acc, app_month, app_date, app_quarter_ending, 
         simd2020_quintile, n_app_patient_same_day) |>
  group_by(dataset_type, app_quarter_ending, simd2020_quintile, hb_name) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop')


df_simd_sco <- df_app |>
  select(all_of(data_keys), ref_acc, app_quarter_ending, 
         simd2020_quintile, n_app_patient_same_day) |>  
  group_by(dataset_type, app_quarter_ending, simd2020_quintile) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |>
  mutate(hb_name = "NHS Scotland") 
  
 df_simd <- rbind(df_simd_hb, df_simd_sco) |>
  #filter(dataset_type == dataset_choice) |> 
  pivot_wider(names_from = simd2020_quintile, values_from = appointments) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |> 
  arrange(hb_name) |>
  adorn_totals("col") |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_simd_quarterly")) #_", dataset_choice
 
 rm(df_simd_hb, df_simd_sco)
#|> 
  # left_join(df_pop_simd, by = c("dataset_type", "simd2020_quintile" = "SIMD2020_5")) |> 
  # mutate(appointment_rate =  round(appointments / population_size * 1000, digits = 1))



#### AGE & SEX #################################################################
# need to use dob rather than age at ref rec?

df_age_hb <- df_app |>
  select(all_of(data_keys), app_date, app_month, sex_reported, age_at_ref_rec, age_group, n_app_patient_same_day) |>  
  group_by(dataset_type, hb_name, sex_reported, age_group) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
  mutate(sex = case_when(
    sex_reported == 1 ~ 'Male',
    sex_reported == 2 ~ 'Female',
    is.na(sex_reported) ~ "Not known",
    TRUE ~ "Other")) |>
   select(-sex_reported) |>
   arrange(dataset_type, hb_name, sex, age_group)


 df_age_sco <- df_app |>
   select(all_of(data_keys), app_date, app_month, sex_reported, age_at_ref_rec, age_group, n_app_patient_same_day) |>  
   group_by(dataset_type, sex_reported, age_group) |>  
   summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') |> 
   mutate(sex = case_when(
     sex_reported == 1 ~ 'Male',
     sex_reported == 2 ~ 'Female',
     is.na(sex_reported) ~ "Not known",
     TRUE ~ "Other"),
     hb_name = "NHS Scotland") |>
   select(-sex_reported) |>
   arrange(dataset_type, sex, age_group)


 df_age <- rbind(df_age_hb, df_age_sco) |>
   group_by(hb_name, dataset_type, sex, age_group) |>
   summarise(appointments = sum(appointments), .groups = "drop") |> # deals with strange duplication of 'other' for sex
   arrange(readr::parse_number(age_group)) |> # orders age range 
   pivot_wider(names_from = age_group, values_from = appointments,
               values_fn = list) |> 
   unnest(cols = everything()) |>
   #filter(dataset_type == dataset_choice) |>
   mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
   arrange(hb_name, dataset_type) |>
   save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_agesex")) #_", dataset_choice
 
 
 

#### NEW AND RETURN ############################################################


df_nr_hb <- df_app |> 
  group_by(dataset_type, hb_name, app_quarter_ending, att_cat) |>  
  summarise(appointments = sum(n_app_patient_same_day), .groups = 'drop') # then sum up apps per day to get total

df_nr_sco <- df_nr_hb |> 
  group_by(dataset_type, app_quarter_ending, att_cat) |> 
  summarise(appointments = sum(appointments, na.rm = TRUE)) |> 
  mutate(hb_name = "NHS Scotland")

df_nr_all <- bind_rows(df_nr_hb, df_nr_sco) |>
  #filter(dataset_type == dataset_choice) |>
  mutate(att_cat = as.factor(att_cat),
         att_cat = recode(att_cat, "1" = "New", "2" = "Return")) |> 
  pivot_wider(names_from = att_cat, values_from = appointments) |> 
  adorn_totals("col") |>
  mutate(app_quarter_ending = format(app_quarter_ending, "%b %Y"), 
         across(New:Total, ~prettyNum(., big.mark = ",")),
         across(.fns = ~replace(., is.na(.), ".."))) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/apps_new_return_quarterly")) #_", dataset_choice

rm(df_nr_hb, df_nr_sco)  

