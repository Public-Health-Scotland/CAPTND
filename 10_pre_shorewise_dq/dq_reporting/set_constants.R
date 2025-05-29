
#####################.
### Set Constants ###
#####################.

# Author: Charlie Smith
# Date: 2024-05-07


captnd_code_lookup <- "../../../data/captnd_codes_lookup.xlsx" # location of lookup file for CAPTND codes


month_latest = ymd(readline(prompt = 'Please enter latest reporting month (YYYY-MM-DD format): '))

month_start <- month_latest - months(14) # start 15 months prior to latest month

vec_timeframe <- seq.Date(from = month_start, # August 2021 = start of SWIFT subs
                          to = month_latest,
                          by = "month")

# Create month date in words for later reference
month_word_start <- date_to_month_year_words(min(vec_timeframe))
month_word_end <- date_to_month_year_words(max(vec_timeframe))


# # Charlies variables
vec_vars <- c("ucpn", "chi", "upi",

              "sex", "dob", "ethnicity", "looked_after_c", "protection", "vet", 
              "preg_perinatal_ref", "postcode", 
              
              "ref_date", "ref_rec_date", 
              #"ref_reason", # removed December 2024
              "ref_acc",  "ref_rej_act", 
              "ref_rej_date", "ref_source", "rej_reason", "act_code_sent_date", 
              "care_plan_inc",
              
              "app_date", "app_purpose", "att_cat", "att_status", "cancellation_date", "location", 
              "prof_group", "preg_perinatal_app", "unav_date_start", "unav_date_end", 
              "unav_days_no", "unav_reason", "presenting_prob_1", "presenting_prob_2",
              "presenting_prob_3", 
              
              #"diag_1", "diag_2", "diag_3", # removed December 2024
              "treat_reason_1", "treat_reason_2", "treat_reason_3",
              "treat_1", "treat_2", "treat_3", 
              "treat_group_or_ind_1", "treat_group_or_ind_2", "treat_group_or_ind_3", 
              "treat_start_date", 
              


              "sex", "dob", "ethnicity", "looked_after_c", "protection", "vet",
              "preg_perinatal_ref", "postcode",

              "ref_date", "ref_rec_date",
              #"ref_reason", # removed December 2024
              "ref_acc",  "ref_rej_act",
              "ref_rej_date", "ref_source", "rej_reason", "act_code_sent_date",
              "care_plan_inc",

              "app_date", "app_purpose", "att_cat", "att_status", "cancellation_date",
              "location",
              "prof_group", "preg_perinatal_app", "unav_date_start", "unav_date_end",
              "unav_days_no", "unav_reason", "presenting_prob_1", "presenting_prob_2",
              "presenting_prob_3",

              #"diag_1", "diag_2", "diag_3", # removed December 2024
              "treat_reason_1", "treat_reason_2", "treat_reason_3",
              "treat_1", "treat_2", "treat_3",
              "treat_group_or_ind_1", "treat_group_or_ind_2", "treat_group_or_ind_3",
              "treat_start_date",

              "cgi_i", "pgi_i", "cgi_s",

              "case_closed_date")



vec_dataset_types <- c("CAMHS", "PT")
vec_value <- c("known", "missing", "invalid", "not known")


level_order_hb2 <- setdiff(level_order_hb, "NHS Scotland")
level_order_hb3 <- c("NHSScotland", level_order_hb2)
level_order_hb2 <- c("NHS Scotland", level_order_hb2) # set NHSScotland to start of vec


# # heatmap colours - new style - changed 29/05/2025
# colors <- c("90 to 100%" = "#9CC951", # green 80%
#             "70 to 89.9%" = "#B3D7F2", # blue
#             "0 to 69.9%" = "#D26146", # rust 80%
#             "Not submitted" = "white",
#             "Supplementary info*" = "grey85",
#             "Not applicable" = "grey60")
# 
# colors_rev <- c("0%" = "#9CC951", # green 80%
#                 ">0-33%" = "#C1DD93", # green 50%
#                 ">33-66%" = "#B3D7F2", # blue 30%
#                 ">66-99%" = "#E39C8C", # rust 50%
#                 "100%" = "#D26146", # rust 80%
#                 "Not submitted" = "white",
#                 "Supplementary info*" = "grey85",
#                 "Not applicable" = "grey60")
# 
# vec_prop_groupings = c(
#   "0%", ">0-33%", ">33-66%", ">66-99%", "100%", "Not submitted", "Supplementary info*",
#   "Not applicable")


# heatmap colours - previous style - superseded 29/05/2025
colors <- c("0%" = "#D26146", # rust 80%
            ">0-33%" = "#E39C8C", # rust 50%
            ">33-66%" = "#B3D7F2", # blue 30%
            ">66-99%" = "#C1DD93", # green 50%
            "100%" = "#9CC951", # green 80%
            "Not submitted" = "white",
            "Supplementary info*" = "grey85",
            "Not applicable" = "grey60")

colors_rev <- c("0%" = "#9CC951", # green 80%
                ">0-33%" = "#C1DD93", # green 50%
                ">33-66%" = "#B3D7F2", # blue 30%
                ">66-99%" = "#E39C8C", # rust 50%
                "100%" = "#D26146", # rust 80%
                "Not submitted" = "white",
                "Supplementary info*" = "grey85",
                "Not applicable" = "grey60")

vec_prop_groupings = c(
  "0%", ">0-33%", ">33-66%", ">66-99%", "100%", "Not submitted", "Supplementary info*",
  "Not applicable")


# # set text font and size for added text
style_text <- createStyle(fontName = 'Arial', fontSize = 11, halign = "left")
style_date <- createStyle(numFmt = "mmm-yy", fontName = 'Arial', fontSize = 11, halign = "left")
style_count <- createStyle(numFmt = "#,##0", fontName = 'Arial', fontSize = 11, halign = "right")
#style_percent <- createStyle(numFmt = "PERCENT", fontName = 'Arial', fontSize = 11, halign = "right")
style_percent <- createStyle(numFmt = "0.0", fontName = 'Arial', fontSize = 11, halign = "right")
style_percent2 <- createStyle(numFmt = "0.0", fontName = 'Arial', fontSize = 11, halign = "center")
style_header <- createStyle(fontName = 'Arial', fontSize = 11, halign = "right")

source("../../../data/secrets.R")
