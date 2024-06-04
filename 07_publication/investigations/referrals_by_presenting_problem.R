#########################################################.
### For publication - referrals by presenting problem ###
#########################################################.

# Author: Bex Madden
# Date: 2024-05-28


source('02_setup/save_df_as_parquet.R')
source('06_calculations/get_latest_month_end.R')


# set DS choice

#dataset_choice <- "PT"

df <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |> 
  mutate(ref_month = floor_date(ref_rec_date, unit = "month"),
         ref_quarter = ceiling_date(ref_month, unit = "quarter") - 1,
         ref_quarter_ending = floor_date(ref_quarter, unit = "month")) 


most_recent_month_in_data <- get_lastest_month_end(df) 

month_end <- floor_date(most_recent_month_in_data, unit = "month")
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")


demographics <- c("sex_reported", "age_at_ref_rec", "simd2020_quintile", "age_group")


df_refs <- df |>
  filter(ref_month %in% date_range) |>
  select(all_of(data_keys), all_of(demographics), ref_acc_last_reported, ref_quarter_ending, ref_reason) |>
  distinct() |>
  mutate(ref_acc_last_reported_o:=case_when(!!sym(ref_acc_last_reported_o)==1 ~ 'accepted',
                                              !!sym(ref_acc_last_reported_o)==2 ~ 'not accepted',
                                              TRUE ~ 'pending'),
         ref_reason = as.factor(ref_reason)) 



reasons <- df_refs |>
  group_by(dataset_type, ref_reason, hb_name, ref_quarter_ending) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
            ref_reason == "99" |
            ref_reason == "98" ~ "invalid_reason",
          TRUE ~ "valid_reason"))

reasons_sco <- df_refs |>
  group_by(dataset_type, ref_reason, ref_quarter_ending) |>
  summarise(n = n()) |>
  ungroup() |>
  mutate(reason_given = case_when(
    is.na(ref_reason) |
      ref_reason == "99" |
      ref_reason == "98" ~ "invalid_reason",
    TRUE ~ "valid_reason"),
    hb_name = "NHS Scotland")

reasons_both <- bind_rows(reasons, reasons_sco) |>
  mutate(hb_name = factor(hb_name, levels = level_order_hb)) |>
  arrange(hb_name) 
  #save_as_parquet() # needed?

rm(reasons, reasons_sco)


reasons_totals <- reasons_both |>  
  #filter(dataset_type == dataset_choice) |>
  group_by(reason_given, hb_name, ref_quarter_ending, dataset_type) |>
  summarise(totals = sum(n)) |>
  ungroup() |>
  group_by(hb_name, dataset_type, ref_quarter_ending) |>
  mutate(all_refs = sum(totals),
         perc_refs = round(totals/all_refs*100, 2)) |>
  pivot_wider(names_from = reason_given, values_from = c("totals", "perc_refs")) |>
  arrange(hb_name, dataset_type) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_given_quarterly")) #_", dataset_choice


# Lookup codes for presenting problem

reason_lookup <- read.xlsx("../../../data/captnd_codes_lookup.xlsx", sheet = "Ref_Reason") %>% 
  select(-Further.Information) |>
  mutate(Code = as.factor(Code)) |>
  rename(reason = Ref_Reason,
         ref_reason = Code)

reasons_labelled <- left_join(reasons_both, reason_lookup, by = "ref_reason") 
# could group reasons together using icd10 https://icd.who.int/browse10/2019/en#/V

#set order of reason codes
reason_order <- reason_lookup$reason

reasons_all <- reasons_labelled |>
  #filter(dataset_type == dataset_choice) |>
  ungroup() |>
  select(-c(reason_given, ref_reason)) |>
  mutate(reason = factor(reason, levels = reason_order),
         reason = fct_na_value_to_level(reason, "N/A")) |>
  arrange(reason) |>
  pivot_wider(names_from = reason, values_from = n) |>
  adorn_totals("col") |>
  arrange(hb_name) |>
  save_as_parquet(paste0(shorewise_pub_data_dir, "/referrals_reason_quarterly")) #_", dataset_choice

