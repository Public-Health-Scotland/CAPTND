##################################################.
#### APPOINTMENT ATTENDANCE by length of wait ####.
##################################################.

# Author: Luke Taylor
# Date: 2026-04-24

# Uses first contact appointment for reporting attendance by length of wait 
# based on ref_rec_date_opti.


df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet'))
  
df_app <- df |>
  mutate(app_month = floor_date(!!sym(app_date_o), unit = "month")) |>
  select(all_of(data_keys), ref_rec_date_opti, !!app_date_o, !!app_month_o, !!att_status_o, 
         !!att_cat_o, !!app_purpose_o, !!ref_acc_o, !!app_date_o) |> 
  filter(!is.na(!!sym(app_date_o)))  


# get first contact appointment date per pathway only
df_first_app <- df_app |>
  arrange(!!!syms(data_keys), !!sym(app_date_o)) |> 
  lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1)|> 
  ungroup() |> 
  as.data.frame() |> 
  #filter(!!sym(app_month_o) %in% date_range) |>
  mutate(Attendance = fcase(
    !!sym(att_status_o) == "1", "Attended",
    !!sym(att_status_o) == "2", "Clinic cancelled",
    !!sym(att_status_o) == "3", "Patient cancelled",
    !!sym(att_status_o) == "5", "Patient CNW",
    !!sym(att_status_o) == "8", "Patient DNA",
    !!sym(att_status_o) == "9", "Patient died",
    !!sym(att_status_o) == "99", "Not known",
    default = "Not recorded")) 


#dna rate by wait for first appt
df_dna_first_appt <- df_first_app |>
  filter(!!sym(app_month_o) %in% date_range) |>
  mutate(wait_for_first_appt = as.numeric(app_date - ref_rec_date_opti),
         wait_cat = case_when(wait_for_first_appt <= 126 ~ "0 to 18 weeks",
                              wait_for_first_appt > 126 & wait_for_first_appt <= 245 ~ "19 to 35 weeks",
                              wait_for_first_appt > 245 & wait_for_first_appt <= 364 ~ "36 to 52 weeks",
                              wait_for_first_appt > 364 ~ "Over 52 weeks")) |>
  group_by(dataset_type, wait_cat, Attendance) |>
  summarise(count = n()) |>
  group_by(dataset_type, wait_cat) |>
  mutate(total_apps = sum(count),
         att_rate = round(count/total_apps*100,1))
  
