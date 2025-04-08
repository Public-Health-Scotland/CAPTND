#####################################################.
##### For publication - adult protection status #####
#####################################################.

# Author: Luke Taylor
# Date: 2024-11-27

summarise_referrals_prot <- function(){
  
ref_prot_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/")
dir.create(ref_prot_dir)
measure_label <- "referrals_prot_"


# single row per individual
df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  filter(!!sym(referral_month_o) %in% date_range) |> 
  mutate(ref_quarter = ceiling_date(referral_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) |> 
  #lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  fill("protection", .direction = "downup") |>
  slice(1) |> 
  ungroup() |> 
  #as.data.frame() |> 
  add_sex_description() |> 
  tidy_age_group_order() |>
  remove_borders_int_refs()

#set order of reason codes
prot_order <- c('Yes', 'No', 'Not known', 'Data missing')

##Adult Protection Status##
# overall -----------------------------------------------------------------

# by hb
df_all_hb <- df_single_row |> 
  filter(!!sym(dataset_type_o) == 'PT') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(protection_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                               !!sym(protection_o) == 2 ~ 'Yes',
                               !!sym(protection_o) == 99 ~ 'Not known',
                               is.na(!!sym(protection_o)) ~ 'Data missing')) |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         prot_label = factor(prot_label, levels = prot_order)) |> 
  arrange(!!sym(hb_name_o), prot_label) |> 
  save_as_parquet(path = paste0(ref_prot_dir, measure_label, "adult_all_hb"))


#by quarter hb
df_qr_hb <- df_single_row |> 
  filter(!!sym(dataset_type_o) == 'PT') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(protection_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                               !!sym(protection_o) == 2 ~ 'Yes',
                               !!sym(protection_o) == 99 ~ 'Not known',
                               is.na(!!sym(protection_o)) ~ 'Data missing')) |>
  select(ref_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         prot_label = factor(prot_label, levels = prot_order)) |> 
  arrange(!!sym(hb_name_o), ref_quarter_ending, prot_label) |> 
  save_as_parquet(path = paste0(ref_prot_dir, measure_label, "adult_qr_hb"))



##Child Protection Status##
# overall -----------------------------------------------------------------

# by hb
df_all_hb <- df_single_row |> 
  filter(!!sym(dataset_type_o) == 'CAMHS') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o)) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(protection_o)) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                                !!sym(protection_o) == 2 ~ 'Yes',
                                !!sym(protection_o) == 99 ~ 'Not known',
                                is.na(!!sym(protection_o)) ~ 'Data missing')) |>
  select(!!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         prot_label = factor(prot_label, levels = prot_order)) |> 
  arrange(!!sym(hb_name_o), prot_label) |> 
  save_as_parquet(path = paste0(ref_prot_dir, measure_label, "child_all_hb"))


#by quarter hb
df_qr_hb <- df_single_row |> 
  filter(!!sym(dataset_type_o) == 'CAMHS') |>
  group_by(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(protection_o), ref_quarter_ending) |> 
  summarise(count = n(), .groups = "drop") |>
  group_by(!!sym(dataset_type_o), !!sym(protection_o), ref_quarter_ending) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(!!sym(hb_name_o), ~"NHS Scotland"),
                      .groups = "drop")) |> 
  ungroup() |>
  mutate(prot_label = case_when(!!sym(protection_o) == 1 ~ 'No',
                                !!sym(protection_o) == 2 ~ 'Yes',
                                !!sym(protection_o) == 99 ~ 'Not known',
                                is.na(!!sym(protection_o)) ~ 'Data missing')) |>
  select(ref_quarter_ending, !!sym(dataset_type_o), !!sym(hb_name_o), prot_label, count) |>
  mutate(!!sym(hb_name_o) := factor(!!sym(hb_name_o), levels = hb_vector),
         prot_label = factor(prot_label, levels = prot_order)) |> 
  arrange(!!sym(hb_name_o), ref_quarter_ending, prot_label) |> 
  save_as_parquet(path = paste0(ref_prot_dir, measure_label, "child_qr_hb"))

}
