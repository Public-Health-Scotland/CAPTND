
###########################################.
### CAPTND Modelling: Survival Analysis ###
###########################################.

# Author: Charlie Smith
# Date: 2024-10-30




# 1 - Housekeeping --------------------------------------------------------


month_end <- "2024-09-01"

source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

sub_month_end <- ymd(month_end)
sub_month_start <- ymd(month_end) - months(14)

month_seq <- seq.Date(from = ymd(sub_month_start), to = ymd(sub_month_end), by = "month")
df_month_seq_end <- data.frame(sub_month_end = ceiling_date(month_seq, unit = "month")-1) # month_last_day

month_range <- seq.Date(from = sub_month_end-months(14), to = sub_month_end, by = "month")

dir.create(pat_waits_dir)
measure_label <- "patients_wait_"




# 2 - Get data ------------------------------------------------------------

df_single_row <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) |>
  #filter(ucpn == "1280128683654") |> 
  arrange(!!sym(header_date_o)) |> 
  group_by(across(all_of(data_keys))) |> 
  mutate(ever_una = case_when(
    !is.na(unav_date_start) | !is.na(unav_days_no_o) ~ 1,
    TRUE ~ NA), 
    ever_dna = if_else(att_status == 8, 1, NA)) |> 
  fill(ever_una, .direction = "downup") |> 
  fill(ever_dna, .direction = "downup") |> 
  mutate(ever_una = if_else(is.na(ever_una), 0, ever_una),
         ever_dna = if_else(is.na(ever_dna), 0, ever_dna)) |> 
  fill(!!sym(first_treat_app_o), .direction = "downup") |> 
  slice(1) |> 
  ungroup() |> 
  cross_join(df_month_seq_end)


df_waits <- df_single_row |> 
  mutate(off_list_date = coalesce(!!sym(first_treat_app_o), !!sym(case_closed_date_o),
                                  !!sym(act_code_sent_date_o)),
         sub_month_start = floor_date(sub_month_end, unit = "month"),
         off_list_month_end = as.Date(ceiling_date(off_list_date, unit = "month")-1), 
         rej_month_end = as.Date(ceiling_date(ref_rej_date, unit = "month")-1), 
         
         # add wait status
         wait_status = case_when(
           !is.na(ref_rej_date) & rej_month_end <= sub_month_end ~ "rejected",
           ref_rec_date_opti <= sub_month_end & is.na(off_list_date) ~ "on list",
           off_list_month_end == sub_month_end ~ "tx Start",
           off_list_date > sub_month_end & ref_rec_date_opti < sub_month_end ~ "on list",
           TRUE ~ NA_character_)) |> 
  mutate(wait_unadj_days = case_when(
    wait_status == "on list" ~ round((sub_month_end-ref_rec_date_opti), 1),
    wait_status == "tx Start" ~ round((off_list_date-ref_rec_date_opti), 1)),
    wait_unadj_weeks = as.numeric(round(wait_unadj_days / 7, 1))) |> 
  filter(wait_status %in% c("on list", "tx Start") &
          ref_acc_last_reported %in% c(1, 3)) |> # must be accepted or pending (rejected would never be seen so not fair to include)
  group_by(across(all_of(data_keys))) |> 
  slice(n()) |> 
  ungroup() |> 
  mutate(wait_status_bi = case_when(
    wait_status == "on list" ~ 0,
    wait_status == "tx Start" ~ 1)) |>  
  
  select(header_date, dataset_type, hb_name, patient_id, ucpn, sex_reported, 
         age_group, simd2020_quintile,  ethnicity_last_reported, 
         protection, looked_after_c_edited, vet_edited, preg_perinatal, 
         
         ref_rec_date_opti, referral_month, has_act_code_sent_date, 
         
         app_month, ever_dna, ever_una,
         location, wait_status_bi, wait_unadj_weeks)

save_as_parquet(df = df_waits, 
                path = "../../../data/surv_data")

# select relevant cols:
# ever DNA?
# ever unavailable?
# urban-rural?

# date to include data from? 
# must have been accepted or pending

# 3 - Survival analysis ---------------------------------------------------


# 3.1 - load survival packages --------------------------------------------
# install.packages(c("ggsurvfit", "survminer", "survival"))

library(ggsurvfit)
library(survminer)
library(survival)


# 3.2 - load data ---------------------------------------------------------

df_waits <- read_parquet("../../../data/surv_data.parquet") 


# 3.3 - create survival object --------------------------------------------
df_s <- Surv(time = df_waits$wait_unadj_weeks,
             event = df_waits$wait_status_bi)



# 3.4 - create basic survival curve ---------------------------------------

s1 <- survfit(df_s ~ 1, df_waits)

ggsurvfit(s1, linewidth = 1)+
  labs(x = "weeks", y = "waiting probability")+
  add_confidence_interval()+
  add_risktable()+
  scale_ggsurvfit()

summary(s1, times = 18)



# 3.5 - K-M curve for groups ----------------------------------------------

s2 <- survfit(df_s ~ df_waits$simd2020_quintile )

ggsurvfit(s2)+
  labs(x = "weeks", y = "waiting probability")+
  theme(legend.position = "right")



log_rank_simd <- survdiff()




         
         