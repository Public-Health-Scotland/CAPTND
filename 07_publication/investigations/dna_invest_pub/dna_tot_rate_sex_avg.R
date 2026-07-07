

tot_dna_rate_sex_avg <- function(dataset_choice){
  
  df_tot_dna_sex <- read_parquet(paste0(apps_att_dir, "total_dnas_", "qt_hb_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_apps_att, -total_sex, -app_quarter_ending) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, sex_reported) |>
    mutate(apps_att = sum(apps_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), sex_reported) |>
    mutate(tot_apps = sum(apps_att),
           att_rate = round(apps_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA',
           sex_reported == 'Male' | sex_reported == 'Female',
           dataset_type == dataset_choice) |>
    select(dataset_type, hb_name, Attendance, att_rate) |>
    pivot_wider(names_from = 'sex_reported', values_from = 'att_rate')
  
  return(df_tot_dna_sex)

}