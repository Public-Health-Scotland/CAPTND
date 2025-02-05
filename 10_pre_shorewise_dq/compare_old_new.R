
##########################################.
### Compare new and old DQ report data ###
##########################################. 

# Author: Charlie Smith
# Date: 2024-10-31


df_old <- import(file = "../../../../R script/CAPTND Data Quality/Data Quality Report/Output/dq_reports_new_2024-11-01/CAPTND_DQ_report_2024-11-01_update.xlsx",
                 which = "Heatmap Data") |> 
  tail(-10) |> 
  row_to_names(1) |> 
  mutate(Month = as.Date(as.numeric(Month), origin = "1899-12-30"),
         Variable = case_when(
           Variable == "UCPN" ~ "ucpn",
           Variable == "UPI" ~ "upi",
           Variable == "CHI" ~ "chi",
           
           Variable == "RECEIVED_DATE" ~ "ref_rec_date",
           Variable == "POSTCODE" ~ "postcode",
           
           Variable == "SEX" ~ "sex",
           Variable == "ETHNICITY" ~ "ethnicity",
           Variable == "DOB" ~ "dob",
           Variable == "REF_DATE" ~ "ref_date",
           Variable == "PROTECTION" ~ "protection",
           
           Variable == "LAC" ~ "looked_after_c",
           Variable == "VETERAN" ~ "vet",
           Variable == "PPMH_REF" ~ "preg_perinatal_ref",
           Variable == "REF_SOURCE" ~ "ref_source",
           Variable == "REF_REASON" ~ "ref_reason",
           
           Variable == "ACCEPTED" ~ "ref_acc",
           Variable == "REJ_DATE" ~ "ref_rej_date",
           Variable == "REJ_REASON" ~ "rej_reason",
           Variable == "REJ_ACTIONS" ~ "ref_rej_act",
           Variable == "CODE_SENT_DATE" ~ "act_code_sent_date",
           
           Variable == "APP_DATE" ~ "app_date",
           Variable == "APP_PURPOSE" ~ "app_purpose",
           Variable == "ATT_STATUS" ~ "att_status",
           Variable == "ATT_CATEGORY" ~ "att_cat",
           Variable == "PROF_GROUP" ~ "prof_group",
           
           Variable == "LOCATION" ~ "location",
           Variable == "PPMH_APP" ~ "preg_perinatal_app",
           Variable == "UNA_START_DATE" ~ "unav_date_start",
           Variable == "UNA_END_DATE" ~ "unav_date_end",
           Variable == "NUM_DAYS" ~ "unav_days_no",
           
           Variable == "UNA_REASON" ~ "unav_reason",
           Variable == "DIAG1" ~ "diag_1",
           Variable == "TREAT1" ~ "treat_1",
           Variable == "GRPIND1" ~ "treat_group_or_ind_1",
           Variable == "START_DATE" ~ "treat_start_date",
          
           Variable == "DISCHARGE_DATE" ~ "case_closed_date")) |> 
  rename(`DQ Assessment` = Value) |> 
  select(c("Month", "Dataset", "Health Board", "Variable", `DQ Assessment`, "Count")) |> 
  mutate(Count = if_else(Count == "-", NA_character_, Count),
         Count = as.numeric(Count),
         `DQ Assessment` = tolower(`DQ Assessment`))

# unique(df_old$Variable)
# unique(df_new$Variable)

df_new <- import(file = "../../../output/analysis_2025-02-05/report_creation/dq_report_2024-11-01_new.xlsx",
                 which = "Heatmap Data") |> 
  tail(-10) |> 
  row_to_names(1) |> 
  mutate(Month = as.Date(as.numeric(Month), origin = "1899-12-30")) |> 
  select(c("Month", "Dataset", "Health Board", "Variable", `DQ Assessment`, "Count")) |> 
  mutate(Count = if_else(Count == "-", NA_character_, Count),
         Count = as.numeric(Count))

# join
df_joined <- full_join(df_new, df_old, by = c("Month", "Dataset", "Health Board",
                                              "Variable", 'DQ Assessment'),
                       suffix = c("_new", "_old")) |> 
  filter(`Health Board` != "NHS Scotland") |> 
  mutate(diff = Count_new - Count_old) |> 
  group_by(Dataset, `Health Board`) |> 
  arrange(Dataset, `Health Board`, diff) |> 
  filter(! Variable %in% c("ucpn", "chi", "upi") & # can remove these as differences expected
           Variable != "postcode" & # old DQ report uses old lookup, so will be some disparities
           diff != 0 ) # remove those that match perfectly

# test for issues
if(nrow(df_joined) == 0 ) {
  
  message("Old and new DQ reports adequately match!")
  
} else {
  
  message("Old and new DQ reports do not adequately match! Looking into this...")
  View(df_joined)
  
}






