################################################.
### For publication - referrals by perinatal ###
################################################.

# Author: Bex Madden
# Date: 2024-11-28


summarise_referrals_ppmh <- function(){
  
ref_ppmh_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/")
dir.create(ref_ppmh_dir)
measure_label <- "referrals_ppmh_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range) |> # apply date range filter
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  add_sex_description() |> 
  mutate(preg_perinatal = case_when(preg_perinatal == 1 ~ "Not pregnant",
                                    preg_perinatal == 2 ~ "Currently pregnant",
                                    preg_perinatal == 3 ~ "Post-natal (within 12m)",
                                    preg_perinatal == 96 ~ "Not applicable",
                                    TRUE ~ "Not known")) 


# are we meant to distinguish between ppmh at ref and app? submitted separately where are they concatenated?

# overall -----------------------------------------------------------------
# by hb
df_all_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o)) |> #, !!sym(dataset_type_o)
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "all_hb"))

# by hb and sex
df_all_hb_sex <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o)) |> #, !!sym(dataset_type_o)
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "all_hb_sex")) #maleness dealt with differently - some not applicable, some not pregnant, some left as NA - argument for combining all non-pregnancies?


# quarterly ----------------------------------------------------------------

# by hb
df_qt_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> 
  arrange(!!sym(hb_name_o)) |> #, !!sym(dataset_type_o)
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "qt_hb"))

# by hb and sex
df_qt_hb_sex <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(preg_perinatal_o), !!sym(sex_reported_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  filter(dataset_type == "PT") |> # sex_reported == "Female"
  arrange(!!sym(hb_name_o), ref_quarter_ending) |> #, !!sym(dataset_type_o)
  save_as_parquet(path = paste0(ref_ppmh_dir, measure_label, "qt_hb_sex")) 



}

