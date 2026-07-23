#############################################################################.
### Table: first contact appointment attendance, 15mth publication period ###
#############################################################################.

# Author: Luke Taylor
# Date: 2026-07-13

create_table_firstcon_att_pub_period <- function(){
  
  first_att_latest <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_firstcon/apps_firstcon_qt_hb.parquet")) |>
    select(-prop_firstcon_att, -app_quarter_ending) |> 
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o), Attendance) |>
    mutate(firstcon_att = sum(firstcon_att)) |>
    group_by(!!sym(dataset_type_o), !!sym(hb_name_o)) |>
    mutate(first_contact = sum(unique(first_contact)),
           total_apps = sum(unique(total_apps))) |>
    filter(Attendance == 'Patient DNA') |>
    mutate(firstcon_att = replace_na(firstcon_att, 0),
           prop_firstcon_dna = sprintf("%.1f%%", firstcon_att / first_contact * 100),
           across(firstcon_att:total_apps, ~prettyNum(., big.mark = ","))) |>
    ungroup() |>
    distinct() |>
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
    mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |>  
    select(dataset_type, hb_name, total_apps, first_contact, firstcon_att, prop_firstcon_dna) |>
    
    rename(`Health board` = !!sym(hb_name_o),
           `Total appointments` = total_apps,
           `1st contact appointments` = first_contact,
           `1st contact DNA` = firstcon_att,
           `1st contact DNA rate` = prop_firstcon_dna) |>
    filter(!is.na(`Health board`))  # remove empty nhs 24 row
  
  
  first_att_latest[is.na(first_att_latest)] <- ".." # make NAs '..'
  first_att_latest[first_att_latest == "0"] <- "-" # make 0 '-'
  first_att_latest[first_att_latest == "0.0%"] <- "-" # make 0% '-'
  
  save_as_parquet(first_att_latest, paste0(shorewise_pub_data_dir, "/appointments_firstcon/table_firstcon_att_latest_pub_period")) # _", dataset_choice)) 
}


