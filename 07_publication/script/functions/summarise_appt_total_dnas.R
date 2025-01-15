#####################################
########## DNAs by Quarter ##########
#####################################

#Author: Luke Taylor
#Date: 18/12/2024

summarise_total_dnas <- function(df){

#most recent quarter end
quarter_end_date <- ymd("2024/09/01")

# create for for saving output files in
apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
dir.create(apps_att_dir)

# measure labels
measure_label <- "dnas_" # for file names

# get appointments df
df_app <- get_appointments_df() 


#Total Appts by Quarter
df_tot_app_qt <- df_app |>
  filter(!!sym(app_month_o) %in% date_range) |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>  
  summarise(total_apps = sum(n_app_patient_same_day), .groups = 'drop') |> 
  group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  ungroup()


#All DNAs by Quarter
df_tot_dnas_qt <- df_app |> 
  filter(!!sym(app_month_o) %in% date_range,
         !!sym(att_status_o) == "8") |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), app_quarter_ending) |>
  summarise(dna_count = sum(n_app_patient_same_day), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), app_quarter_ending) %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |>
  left_join(df_tot_app_qt, by = c("dataset_type", "hb_name", "app_quarter_ending")) |>
  mutate(dna_rate = round(dna_count/total_apps, 4)) |>
  save_as_parquet(path = paste0(apps_att_dir, measure_label, "total_qr_hb"))

}