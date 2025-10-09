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
  arrange(ucpn, app_date) |>
  lazy_dt() |>
  group_by(!!!syms(data_keys)) |>
  fill("looked_after_c_edited", .direction = "downup") |>
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  mutate(looked_after_c_edited = case_when(looked_after_c_edited == 1 ~ "No",
                                           looked_after_c_edited == 2 ~ "Yes",
                                           TRUE ~ "Not known")) |>
  remove_borders_int_refs()


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

# monthly --------------------------------------------------------------

# by hb
df_mth_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(looked_after_c_edited_o), referral_month) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(looked_after_c_edited_o), referral_month) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "CAMHS") |> 
  select(referral_month, !!sym(dataset_type_o), !!sym(hb_name_o), !!sym(looked_after_c_edited_o), count) |>
  right_join(df_lac_mth_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name", "looked_after_c_edited")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  arrange(!!sym(hb_name_o), referral_month) |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |>
  mutate(total = sum(count)) |> ungroup() |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |>
  save_as_parquet(path = paste0(ref_lac_dir, measure_label, "mth_hb"))



}

