#####################################################################.
### Create Table: total appointments + attendance, latest quarter ###
#####################################################################.

# Author: Bex Madden
# Date: 2025-01-13

create_table_app_att <- function(){
  
  app_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_qt_hb.parquet")) |> 
    select(-prop_apps_att) |> 
    ungroup() |> 
    #pivot_wider(names_from = Attendance, values_from = firstcon_att) |> 
    filter(app_quarter_ending == month_end,
           Attendance == "Patient DNA") |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), total_apps, apps_att) |> 
    mutate(apps_att = replace_na(apps_att, 0),
           prop_app_dna = round(apps_att/total_apps*100, 1),
           prop_app_dna = paste0(prop_app_dna, "%"),
           across(total_apps:apps_att, ~prettyNum(., big.mark = ","))) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in any missing rows
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    
    rename(`Health board` = !!sym(hb_name_o),
           `Total appointments` = total_apps,
           `DNA total appointments` = apps_att,
           `Total DNA rate` = prop_app_dna) |>
    filter(!is.na(`Health board`))  # remove empty nhs 24 row
  
  
  app_att_latest[is.na(app_att_latest)] <- ".." # make NAs '..'
  app_att_latest[app_att_latest == "0"] <- "-" # make 0 '-'
  app_att_latest[app_att_latest == "0%"] <- "-" # make 0% '-'
  
  save_as_parquet(app_att_latest, paste0(apps_att_dir, "table_app_att_latest_qt")) # _", dataset_choice)) 
}
