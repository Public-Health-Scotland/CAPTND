
#####################.
### Set constants ###
#####################.

# Author: Charlie Smith
# Date: 2023-11-14




# 1 - Establish data time frame -------------------------------------------

month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")
vec_timeframe = date_range
range_12_month <- seq.Date(from = month_end - months(11), to = month_end, by = "month")

# 2 - Production data -----------------------------------------------------
publication_month <- month_end + months(3)


production_date <- Sys.Date()
production_month <- format(as.Date(production_date), "%B %Y")

# 3 - Set safe place to save working data ---------------------------------

ref_dir <- paste0(shorewise_pub_data_dir, "/referrals/")
ref_source_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/")
ref_demo_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_demographics/")
non_acc_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance/")
non_acc_reason_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance_reason/")
non_acc_action_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance_action/")
apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
basic_opti_dir <- paste0(shorewise_pub_data_dir, "/basic_v_opti/")
markdown_dir <- paste0(root_dir, "/markdown/")
open_dir <- paste0(shorewise_pub_data_dir, "/open_cases/")
pat_waits_dir <- paste0(shorewise_pub_data_dir, "/patients_waiting/")
pat_seen_dir <- paste0(shorewise_pub_data_dir, "/patients_seen/")
ref_lac_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_lac/")
ref_prot_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/")
ref_care_plan_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_care_plan/")
ref_vets_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_vet_status/")
treat_reason_dir <- paste0(shorewise_pub_data_dir, "/treat_reason/")
present_prob_dir <- paste0(shorewise_pub_data_dir, "/present_prob/")
treat_intervention_dir <- paste0(shorewise_pub_data_dir, "/treat_intervention/")
clinical_outcomes_dir <- paste0(shorewise_pub_data_dir, "/clinical_outcomes/")
group_ind_dir <- paste0(shorewise_pub_data_dir, "/treat_group_ind/")
ref_ppmh_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/")
dis_dir <- paste0(shorewise_pub_data_dir, "/discharges/")

# 4 - Reference -----------------------------------------------------------

# create HB vector
hb_vector <- c("NHS Ayrshire and Arran", 
               "NHS Borders", 
               "NHS Dumfries and Galloway",
               "NHS Fife",
               "NHS Forth Valley", 
               "NHS Grampian",
               "NHS Greater Glasgow and Clyde",
               "NHS Highland",
               "NHS Lanarkshire", 
               "NHS Lothian", 
               "NHS Orkney",
               "NHS Shetland", 
               "NHS Tayside", 
               "NHS Western Isles",
               "NHS 24",
               "NHS Scotland")


vec_dataset_type <- c("CAMHS", "PT")



vec_vars <- c("ucpn", "chi", "upi", "patient_id",
              
              "sex", "sex_reported", "dob", "dob_verified", "age_at_ref_rec", 
              "ethnicity", "ethnicity_last_reported", "postcode", "postcode_last_reported", 
              "simd2020_quintile", "looked_after_c", "looked_after_c_edited", "protection", "vet", "vet_edited",
              "preg_perinatal_ref", 
              
              "ref_date", "ref_rec_date", "ref_rec_date_opti", "ref_reason", "ref_acc", "ref_rej_act", 
              "ref_rej_date", "ref_source", "rej_reason", "act_code_sent_date", 
              
              "app_date", "app_purpose", "att_cat", "att_status", "location", 
              "prof_group", "preg_perinatal_app", "unav_date_end", "unav_date_start", 
              "unav_days_no", "unav_reason", 
              
              "diag_1", "diag_2", "diag_3", 
              "treat_1", "treat_2", "treat_3", "treat_group_or_ind_1", "treat_group_or_ind_2",
              "treat_group_or_ind_3", "treat_start_date", 
              
              "case_closed_date")



# create complete ds 
df_ds_hb_name <- cross_join(as.data.frame(vec_dataset_type), 
                            as.data.frame(hb_vector)) |> 
  rename(dataset_type = vec_dataset_type,
         hb_name = hb_vector) |> 
  mutate(hb_name = factor(hb_name, levels = hb_vector))
  # filter(!(#dataset_type == "CAMHS" & 
  #            hb_name == "NHS 24")) # remove invalid combo

# with time columns
df_time <- data.frame(month = date_range) |> 
  append_quarter_ending(date_col = "month")

df_months <- df_time |> select(month)
df_quarts <- df_time |> select(quarter_ending)

df_qt_ds_hb <- df_ds_hb_name |> cross_join(df_quarts) |> distinct()
df_month_ds_hb <- df_ds_hb_name |> cross_join(df_months)



# 5. Plotting constants -------------------------------------------------------

# chart dimensions
chart_width = 18 # changed to scale better on a4 page # 24
chart_height = 12 # changed to scale better on a4 page # 16
bar_width = 0.5

# custom plotting theme (does not set legend formatting, 
# requires panel.grid.major.x/y to be manually defined depending on orientation of plot)

theme_captnd <- function(){
  font <- "Arial"
  theme_minimal() %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "grey95", fill = NA, linewidth = 0.75),
      plot.caption = element_text(size = 10, hjust = 1, margin = margin(t = 10)),
      axis.title.x = element_text(size = 12, face = "bold",
                                  margin = margin(t = 15)),
      axis.title.y = element_text(size = 12, face = "bold",
                                  margin = margin(r = 15), angle = 90),
      axis.text.x = element_text(size = 11, color = "black"),
      axis.text.y = element_text(size = 11, color = "black", hjust = 1),
      axis.ticks = element_line(colour = "grey90")
    )
}

  

# 6 - Get date labels for publication doc ----------------------------

# date labels
date_label <- format(as.Date(month_end), "%B %Y")
date_label_qt <- format((as.Date(month_end) %m-% months(3)), "%B %Y")
date_label_yr <- format((as.Date(month_end) %m-% months(12)), "%B %Y")
date_label_5qt <- format((as.Date(month_end) %m-% months(14)), "%B %Y")

# set pub_date as 3 months after month_end
pub_month <- as.Date(month_end %m+% months(3))

## Get next pub date and prev pub date
# (6 months after pub_date, 1st Tuesday)
next_pub_date <- id_next_pub(pub_month)
# (6 months after 1yr before pub_date, 1st Tuesday)
prev_pub_date <- id_next_pub(pub_month - months(12))

#pub_date for this publication itself
pub_date <- id_next_pub(latest_month = pub_month -months(6))
pub_date_dmy <- id_next_pub_date_dmy(latest_month = pub_month -months(6))



# 7 - Constants for data tables -------------------------------------------

source("../../../data/secrets.R")

# 
# dq_template_1 = "dq_info_only_template.xlsx" # DQ Report template
# dq_template_1_update = "dq_report_template_DO_NOT_EDIT.xlsx"
# dq_template_2 = "record_trend_only_template.xlsx" # Record Counts Report template
# 
# # set text font and size for added text
style_text <- createStyle(fontName = 'Arial', fontSize = 11)
style_date <- createStyle(numFmt = "mmm-yy", fontName = 'Arial', fontSize = 11, halign = "left")
style_count <- createStyle(numFmt = "#,##0", fontName = 'Arial', fontSize = 11, halign = "right")
style_percent <- createStyle(numFmt = "PERCENT", fontName = 'Arial', fontSize = 11, halign = "right")


# 8 - Skeleton dataframes for age groups and SIMD

#Age group
age_groups_df <- data.frame(dataset_type = c('CAMHS', 'PT'),
                            agg_age_groups = c('Under 6', 'Under 25', '6-11', '25-39',
                                               '12-15', '40-64', 'Over 15', '65 plus',
                                               'Data missing', 'Data missing')) |> arrange(dataset_type)

df_age_mth_ds_hb <- df_month_ds_hb |>
  full_join(age_groups_df, by = 'dataset_type', relationship = "many-to-many")


#SIMD quintiles
simd_df <- data.frame(simd2020_quintile = c('1 - Most Deprived','2', '3', '4',
                                            '5 - Least Deprived', 'Data missing', 'Not known'))

df_simd_mth_ds_hb <- df_month_ds_hb |>
  cross_join(simd_df)

#Child protection/LAC
lac_df <- data.frame(looked_after_c_edited = c('Yes','No', 'Not known', 'Data missing'))

df_lac_mth_ds_hb <- df_month_ds_hb |>
  cross_join(lac_df) |>
  filter(dataset_type == 'CAMHS')

#Adult protection/Veteran
adult_prot_df <- data.frame(prot_label = c('Yes','No', 'Not known', 'Data missing'))

df_ap_mth_ds_hb <- df_month_ds_hb |>
  cross_join(adult_prot_df) |>
  filter(dataset_type == 'PT')

#Care plan inclusion
care_plan_df <- data.frame(care_plan_inc = c('Yes','No', 'Not known', 'Data missing'))

df_care_mth_ds_hb <- df_month_ds_hb |>
  cross_join(care_plan_df)

#Outcome measures
create_outcomes_df <- function(outcome_measures = c("CGI-I", "PGI-I", "CGI-S")){
  
  measure_df <- data.frame(outcome_code = c('00','01','02','03','04','05',
                                            '06','07')) |>
    mutate(outcome = outcome_measures)

}

cgi_i <- create_outcomes_df("CGI-I")
pgi_i <- create_outcomes_df("PGI-I")
cgi_s <- create_outcomes_df("CGI-S")

df_cgi_i_ds_hb <- df_month_ds_hb |>
  cross_join(cgi_i)

df_pgi_i_ds_hb <- df_month_ds_hb |>
  cross_join(pgi_i)

df_cgi_s_ds_hb <- df_month_ds_hb |>
  cross_join(cgi_s)

# 9 - Constants for dq  ---------------------------------------------------

captnd_code_lookup <- "../../../data/captnd_codes_lookup.xlsx"

