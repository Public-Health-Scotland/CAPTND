#################################################.
### For publication - referrals by lac status ###
#################################################.
#
# Author: Bex Madden
# Date: 2024-11-28


summarise_referrals_lac <- function(){
  
ref_lac_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_lac/")
dir.create(ref_lac_dir)
measure_label <- "referrals_lac_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  #filter(ref_quarter_ending == "2024-12-01" & hb_name == "NHS Greater Glasgow and Clyde") |> 
  lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  mutate(looked_after_c_edited = case_when(looked_after_c_edited == 1 ~ "No",
                                           looked_after_c_edited == 2 ~ "Yes",
                                           TRUE ~ "Not known"))


# overall -----------------------------------------------------------------

# by hb
df_all_hb <- df_single_row  |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(looked_after_c_edited_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(looked_after_c_edited_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 

  filter(dataset_type == "CAMHS") |> 
  arrange(!!sym(hb_name_o)) |> #, !!sym(dataset_type_o)
  save_as_parquet(path = paste0(ref_lac_dir, measure_label, "all_hb"))


# quarterly --------------------------------------------------------------

# by hb
df_qt_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(looked_after_c_edited_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(looked_after_c_edited_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "CAMHS") |> 
  arrange(!!sym(hb_name_o), ref_quarter_ending) |> #, !!sym(dataset_type_o)
  save_as_parquet(path = paste0(ref_lac_dir, measure_label, "qt_hb"))



}

