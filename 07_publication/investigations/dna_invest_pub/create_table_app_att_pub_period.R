###############################################################################.
### Create Table: total appointments + attendance, 15mth publication period ###
###############################################################################.

# Author: Luke Taylor
# Date: 2026-07-13

create_table_app_att_pub_period <- function(){
  
  app_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |> 
    ungroup() |> 
    select(-prop_apps_att, -app_quarter_ending) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance) |>
    mutate(apps_att = sum(apps_att)) |>
    filter(Attendance == 'Patient DNA') |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(total_apps = sum(total_apps),
           apps_att = replace_na(apps_att, 0),
           prop_app_dna = sprintf("%.1f%%", apps_att/total_apps*100),
           across(apps_att:total_apps, ~prettyNum(., big.mark = ","))) |>
    ungroup() |>
    distinct() |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), total_apps, apps_att, prop_app_dna) |>
    
    rename(`Health board` = !!sym(hb_name_o),
           `Total appointments` = total_apps,
           `DNA total appointments` = apps_att,
           `Total DNA rate` = prop_app_dna) |>
    filter(!is.na(`Health board`)) # remove empty nhs 24 row
  
  
  app_att_latest[is.na(app_att_latest)] <- ".." # make NAs '..'
  app_att_latest[app_att_latest == "0"] <- "-" # make 0 '-'
  app_att_latest[app_att_latest == "0.0%"] <- "-" # make 0% '-'
  
  save_as_parquet(app_att_latest, paste0(apps_att_dir, "table_app_att_latest_pub_period")) # _", dataset_choice)) 
}




