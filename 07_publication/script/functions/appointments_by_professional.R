###################################################.
#### APPOINTMENT professional- for publication ####.
###################################################.

# Author: Bex Madden
# Date: 2024-11-28


apps_prof_dir <- paste0(shorewise_pub_data_dir, "/appointments_prof/")
dir.create(apps_prof_dir)
measure_label <- "apps_prof_"

# get appointments df
df_app <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  filter(!!sym(app_month_o) %in% date_range) |>
  select(all_of(data_keys), !!app_date_o, !!app_month_o, !!prof_group_o) |> # need to account for multiples
  filter(!is.na(!!sym(app_date_o))) |> 
  lazy_dt() |> 
  group_by(across(all_of(c(data_keys, app_month_o, app_date_o, prof_group_o)))) |> 
  summarise(n_app_patient_same_day = n(), .groups = 'drop') |>
  distinct() |>
  ungroup() |> 
  as.data.frame() |> 
  mutate(app_month = floor_date(!!sym(app_date_o), unit = "month"),
         app_quarter = ceiling_date(app_month, unit = "quarter") - 1,
         app_quarter_ending = floor_date(app_quarter, unit = "month")) 


# lookup codes for care contact location
prof_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Pro_Group") %>% 
  rename(prof_group = Code,
         prof_label = Pro_Group) 

df_app_label <- df_app |> 
  left_join(prof_lookup, by = "prof_group") 


# get total apps for each time period ---------------------------------------
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
app_prof_all <- df_app_label |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), prof_label) |>
  summarise(count = sum(n_app_patient_same_day), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), prof_label) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_all, by = c("dataset_type", "hb_name")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_apps*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o) |>
  save_as_parquet(paste0(apps_prof_dir, measure_label, "all_hb"))


# quarterly ----------------------------------------------------------------
# by hb 
app_prof_qt <- df_app_label |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), prof_label, app_quarter_ending) |>
  summarise(count = sum(n_app_patient_same_day), .groups = 'drop') |>
  ungroup() |> 
  group_by(!!sym(dataset_type_o), prof_label, app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |> # join in total appointment count in time period
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb),
         prop = round(count/total_apps*100, 1)) |> 
  arrange(!!dataset_type_o, !!hb_name_o, app_quarter_ending) |>
  save_as_parquet(paste0(apps_prof_dir, measure_label, "qt_hb"))
