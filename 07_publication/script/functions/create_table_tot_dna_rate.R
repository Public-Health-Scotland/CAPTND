
#####################################################.
### Create table for total appointment DNAs by HB ###
#####################################################.

create_table_total_dnas <- function(){
  
  measure_label <- "dnas_"
  
  # load data
  df_dnas <- read_parquet(paste0(apps_att_dir, measure_label, "total_qr_hb.parquet")) |>
    ungroup() |> 
    filter(app_quarter_ending == max(app_quarter_ending)) |> 
    select(-c(app_quarter_ending, dna_rate)) |> 
    right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> 
    mutate(hb_name = factor(hb_name, levels = hb_vector)) |> 
    arrange(dataset_type, hb_name) |>
    mutate(dna_rate = (round(dna_count/total_apps * 100, 1))) |> 
    change_nhsscotland_label() 
  
  df_dnas <- df_dnas |> 
    mutate(dna_rate = paste0(dna_rate, "%")) |> 
    rename(`Health board` = !!sym(hb_name_o), 
           `Total DNAs` = `dna_count`, 
           `Total Appointments` = `total_apps`,
           `DNA Rate` = `dna_rate`) |> 
    filter(`Health board` != "NHS 24")
  
  df_dnas$`Total DNAs` <- trimws(format(df_dnas$`Total DNAs`, big.mark = ","))
  df_dnas$`Total Appointments` <- trimws(format(df_dnas$`Total Appointments`, big.mark = ","))
  
  df_dnas[df_dnas == "0"] <- "-"
  df_dnas[df_dnas == "NA"] <- ".."
  df_dnas[df_dnas == "0%"] <- "-"
  df_dnas[df_dnas == "NA%"] <- ".."
  
  save_as_parquet(df_dnas, paste0(apps_att_dir, "table_tot_dna_rate"))
  
}

