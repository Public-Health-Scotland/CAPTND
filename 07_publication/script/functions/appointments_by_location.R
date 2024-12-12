################################################.
#### APPOINTMENT location - for publication ####.
################################################.

# Author: Bex Madden
# Date: 2024-11-28


apps_loc_dir <- paste0(shorewise_pub_data_dir, "/appointments_loc/")
dir.create(apps_loc_dir)
measure_label <- "apps_loc_"

# get appointments df
df_app <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  filter(!!sym(app_month_o) %in% date_range) |>
  select(all_of(data_keys), !!app_date_o, !!app_month_o, !!location_o) |> # need to account for multiples
  filter(!is.na(!!sym(app_date_o))) |> 
  lazy_dt() |> 
  group_by(across(all_of(c(data_keys, app_month_o, app_date_o, location_o)))) |> 
  summarise(n_app_patient_same_day = n(), .groups = 'drop') |>
  distinct() |>
  ungroup() |> 
  as.data.frame() |> 
  mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
         app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(app_quarter, unit = "month")) 


# lookup codes for care contact location
loc_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Location") %>% 
  rename(location = Code,
         loc_label = Location) 

df_app_label <- df_app |> 
  left_join(loc_lookup, by = "location") 

# urban rural classification
df_urb_rur <- df |> 
  lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
    slice(1) |> 
    ungroup() |> 
  as.data.frame() |> 
  select(!!!syms(data_keys), ur8_2020_name) |> 
  mutate(u_r_group = case_when(grepl("Urban", ur8_2020_name) ~ "Urban",
                               grepl("Accessible", ur8_2020_name) ~ "Accessible",
                               grepl("Very Remote", ur8_2020_name) ~ "Very Remote",
                               grepl("Remote", ur8_2020_name) ~ "Remote",
                               TRUE ~ "Not known"))


# get total apps for each time period --------------------------------------
#all time
df_tot_app_all <- df_app |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
  summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |>
  group_by(!!sym(dataset_type_o)) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup()

#quarterly
df_tot_app_qt <- df_app |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
  summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup()




# overall ----------------------------------------------------------------
# by hb 
app_loc_all <- df_app_label |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), loc_label) |>
  summarise(count = sum(n_app_patient_same_day), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), loc_label) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_apps*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  save_as_parquet(paste0(apps_loc_dir, measure_label, "all_hb"))

# by urban rural 
app_loc_all_ur <- df_app_label |> 
  left_join(df_urb_rur, by = c("ucpn", "patient_id", "hb_name", "dataset_type")) |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), loc_label, u_r_group) |>
  summarise(count = sum(n_app_patient_same_day), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), loc_label, u_r_group) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  pivot_wider(names_from = u_r_group, values_from = count) |> 
  adorn_totals("col", name = "loc_total") |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  mutate(total_apps = sum(loc_total))|>
  save_as_parquet(paste0(apps_loc_dir, measure_label, "all_hb_urbrur"))
  

# quarterly ----------------------------------------------------------------
# by hb 
app_loc_qt <- df_app_label |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), loc_label, app_quarter_ending) |>
  summarise(count = sum(n_app_patient_same_day), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), loc_label, app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_apps*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o, app_quarter_ending) |>
  save_as_parquet(paste0(apps_loc_dir, measure_label, "qt_hb"))
