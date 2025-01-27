
########################################################################.
### Create Table 4: basic v opti referral comparison, latest quarter ###
########################################################################.

# Author: Bex Madden
# Date: 2024-08-01



create_table_basic_opti <- function(){
  
latest_quart_refs <- read_parquet(paste0(shorewise_pub_data_dir, "/basic_v_opti/refs_basic_opti_quarterly.parquet")) |> 
  filter(`Quarter ending`  == max(`Quarter ending`)) |>
  select(-`Quarter ending`) |>
  mutate(!!sym(hb_name_o) := as.character(!!sym(hb_name_o))) |> 
  #right_join(df_ds_hb_name, by = c("dataset_type", "hb_name")) |> # add in missing row for orkney pt data
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = level_order_hb)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o)) |> 
  change_nhsscotland_label() |> 
  
  rename(`Health board` = !!sym(hb_name_o)) |>
  filter(!is.na(`Health board`))

latest_quart_refs[is.na(latest_quart_refs)] <- ".." # make NAs ..
latest_quart_refs[latest_quart_refs == "NA%"] <- ".." # make NA% ..
latest_quart_refs[latest_quart_refs == "0"] <- "-" # make 0 '-'
latest_quart_refs[latest_quart_refs == "0%"] <- "-" # make 0% '-'

save_as_parquet(latest_quart_refs, paste0(basic_opti_dir, "table_refs_basic_opti_last_quart")) 
}
