##########################################.
#### APPOINTMENT ATTENDANCE - for MMI ####.
##########################################.

# Author: Bex Madden
# Date: 2024-09-18

# Uses first contact appointment for reporting attendance

create_mmi_table_app_att <- function(){
  
  app_att_mth <- read_parquet(paste0(shorewise_pub_data_dir, "/appointments_att/apps_att_mth_hb.parquet")) |> 
    select(-prop_firstcon_att) |> 
    pivot_wider(names_from = Attendance, values_from = firstcon_att) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), app_month, total_apps, first_contact, `Patient DNA`) |> 
    mutate(`Patient DNA` = replace_na(`Patient DNA`, 0),
           prop_firstcon_dna = round(`Patient DNA`/first_contact*100, 1),
           prop_firstcon_dna = paste0(prop_firstcon_dna, "%"),
           across(total_apps:`Patient DNA`, ~prettyNum(., big.mark = ",")),
           !!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), app_month) |> 
    rename(Dataset = dataset_type,
           `Health board` = !!sym(hb_name_o),
           Month = app_month,
           `Total appointments` = total_apps,
           `1st contact appointments` = first_contact,
           `1st contact DNA` = `Patient DNA`,
           `1st contact DNA rate` = prop_firstcon_dna) 
 
#   test <- app_att_mth |> 
#     mutate_at(vars(`Total appointments`:`1st contact DNA rate`), list(~recode(.,"0" = "-")))
  
  app_att_mth[is.na(app_att_mth)] <- ".." # make NAs '..'
  #app_att_mth[app_att_mth == "0"] <- "-" # make 0 '-' ## not working
 # app_att_mth[app_att_mth == "0%"] <- "-" # make 0% '-' ## not working
  
  save_as_parquet(app_att_mth, paste0(apps_att_dir, "table_apps_att_mth_mmi")) # _", dataset_choice)) 
}

