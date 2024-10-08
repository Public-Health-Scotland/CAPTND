
###################################################################.
### Create Table 3: appointments and acceptance, latest quarter ###
###################################################################.

# Author: Bex Madden
# Date: 2024-08-01

create_table_app_att <- function(){

  first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |> 
    select(-prop_firstcon_att) |> 
    pivot_wider(names_from = Attendance, values_from = firstcon_att) |> 
    filter(app_quarter_ending == max(app_quarter_ending)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), total_apps, first_contact, `Patient DNA`) |> 
    mutate(`Patient DNA` = replace_na(`Patient DNA`, 0),
           prop_firstcon_dna = round(`Patient DNA`/first_contact*100, 1),
           prop_firstcon_dna = paste0(prop_firstcon_dna, "%"),
           across(total_apps:`Patient DNA`, ~prettyNum(., big.mark = ","))) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
    rename(`Health board` = !!sym(hb_name_o),
           `Total appointments` = total_apps,
           `1st contact appointments` = first_contact,
           `1st contact DNA` = `Patient DNA`,
           `1st contact DNA rate` = prop_firstcon_dna) |>
    filter(!is.na(`Health board`))  # remove empty nhs 24 row
  #filter(dataset_type = dataset_choice) |> 
  
  first_att_latest[is.na(first_att_latest)] <- ".." # make NAs '..'
  first_att_latest[first_att_latest == "0"] <- "-" # make 0 '-'
  first_att_latest[first_att_latest == "0%"] <- "-" # make 0% '-'
  
  save_as_parquet(first_att_latest, paste0(apps_att_dir, "table_apps_att_latest_qt")) # _", dataset_choice)) 
}

