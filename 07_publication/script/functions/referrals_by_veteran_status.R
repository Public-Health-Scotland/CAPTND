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
  lazy_dt() |> 
  group_by(!!!syms(data_keys)) |> 
  slice(1) |> 
  ungroup() |> 
  as.data.frame() |> 
  add_sex_description() |> 
  tidy_age_group_order()

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

}

### plot -------------------------------------------------------------------
# df_vets <- df_all_hb |> #can show quarter or period total?
#   filter(!!sym(hb_name_o) == 'NHS Scotland') |>
#   group_by(!!sym(hb_name_o)) |>
#   mutate(tot = sum(count),
#          prop = round(count/tot*100,1),
#          count2 = format(count, big.mark = ","),
#          count2 = trimws(count2),
#          label = paste0(prop, "% (", count2, ")"),
#          prop2 = prop / 100)
# 
# extra_space_for_labels = 0.1
# magitude <- 10^(floor(log10(signif(max(df_vets$prop2, na.rm = TRUE), 1))))
# upper_limit <- (ceiling(max(df_vets$prop2, na.rm = TRUE) / magitude) * magitude)+ extra_space_for_labels
# 
# chart <- df_vets |>
#   ggplot(aes(x = fct_rev(vet_label), y = prop2))+
#   geom_bar(stat = "identity", width = bar_width, fill = "#655E9D")+
#   geom_text(aes( label = label), hjust = -0.1, size = 10/.pt)+
#   scale_y_continuous(
#     minor_breaks = NULL,
#     limits = c(0, upper_limit),
#     labels = scales::label_percent(),
#     breaks = scales::breaks_extended(n = 5))+
#   scale_x_discrete(labels = label_wrap(20)) +
#   coord_flip()+
#   labs(
#     y = "Percentage of Referrals",
#     x = "Veterans Status",
#     caption = paste0("CAPTND extract, ", data_analysis_latest_date)) +
#   theme_captnd() +
#   theme(panel.grid.major.x = element_line(),
#         legend.position = "none")





