#####################################################.
### For publication - referrals by veteran status ###
#####################################################.

# Author: Luke Taylor
# Date: 2024-11-27

summarise_referrals_veteran <- function(){
  
ref_vets_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_vet_status/")
dir.create(ref_vets_dir)
measure_label <- "referrals_vets_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range &
           !!sym(dataset_type_o) == 'PT') |> # apply date range filter
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  arrange(dataset_type, ucpn) |>
  lazy_dt() |>
  group_by(!!!syms(data_keys)) |>
  fill("vet_edited", .direction = "downup") |>
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  add_sex_description() |> 
  tidy_age_group_order() |>
  remove_borders_int_refs()

#set order of reason codes
vet_order <- c('Yes', 'No', 'Not known', 'Data missing')

# overall -----------------------------------------------------------------

# by hb
df_all_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(vet_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(vet_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(vet_label = case_when(!!sym(vet_o) == 1 ~ 'No',
                                  !!sym(vet_o) == 2 ~ 'Yes',
                                  !!sym(vet_o) == 99 ~ 'Not known',
                                  is.na(!!sym(vet_o)) ~ 'Data missing')) |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), vet_label, count) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         vet_label = factor(vet_label, levels = vet_order)) |> 
  arrange(!!sym(hb_name_o), vet_label) |> 
  save_as_parquet(path = paste0(ref_vets_dir, measure_label, "all_hb"))


#by quarter hb
df_qr_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(vet_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(vet_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(vet_label = case_when(!!sym(vet_o) == 1 ~ 'No',
                               !!sym(vet_o) == 2 ~ 'Yes',
                               !!sym(vet_o) == 99 ~ 'Not known',
                               is.na(!!sym(vet_o)) ~ 'Data missing')) |>
  select(ref_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), vet_label, count) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         vet_label = factor(vet_label, levels = vet_order)) |> 
  arrange(!!sym(hb_name_o), ref_quarter_ending, vet_label) |> 
  save_as_parquet(path = paste0(ref_vets_dir, measure_label, "qr_hb"))


#by month hb
df_mth_hb <- df_single_row |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(vet_o), referral_month) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(vet_o), referral_month) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(vet_label = case_when(!!sym(vet_o) == 1 ~ 'No',
                               !!sym(vet_o) == 2 ~ 'Yes',
                               !!sym(vet_o) == 99 ~ 'Not known',
                               is.na(!!sym(vet_o)) ~ 'Data missing')) |>
  select(referral_month, !!sym(dataset_type_o), !!sym(hb_name_o), vet_label, count) |>
  right_join(df_ap_mth_ds_hb, by = c("referral_month" = "month", "dataset_type", "hb_name", "vet_label" = "prot_label")) |>
  mutate(count = case_when(is.na(count) ~ 0,
                           TRUE ~ count)) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         vet_label = factor(vet_label, levels = vet_order)) |> 
  arrange(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |> 
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), referral_month) |>
  mutate(total = sum(count)) |> ungroup() |>
  add_proportion_ds_hb(vec_group = c("referral_month", "dataset_type", "hb_name")) |> 
  save_as_parquet(path = paste0(ref_vets_dir, measure_label, "mth_hb"))

}
