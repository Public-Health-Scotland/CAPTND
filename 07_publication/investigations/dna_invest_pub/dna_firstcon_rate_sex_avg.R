

firstcon_dna_rate_sex_avg <- function(dataset_choice){
  
  df_firstcon_dna_sex <- read_parquet(paste0(apps_firstcon_dir, "firstcon_dnas_", "qt_hb_sex.parquet")) |> 
    ungroup() |> 
    select(-total_apps, -prop_firstcon_att, -first_contact, -app_quarter_ending, -app_month) |> 
    filter(!!sym(hb_name_o) == "NHS Scotland") |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance, sex_reported) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    distinct() |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), sex_reported) |>
    mutate(tot_apps = sum(firstcon_att),
           att_rate = round(firstcon_att/tot_apps*100,2)) |>
    filter(Attendance == 'Patient DNA',
           sex_reported == 'Male' | sex_reported == 'Female',
           dataset_type == dataset_choice) |>
    select(dataset_type, hb_name, Attendance, att_rate) |>
    distinct() |>
    pivot_wider(names_from = "sex_reported", values_from = "att_rate")
  
  return(df_firstcon_dna_sex)
  
}

