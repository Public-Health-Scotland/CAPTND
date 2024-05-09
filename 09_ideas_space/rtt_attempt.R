# attempt at waiting times

# source("04_check_modify/add_rtt_eval.R")
# add_rtt_eval(df)

camhs <- read.csv("bex_test/camhs_adjRTT_test.csv")
pt <- read.csv("bex_test/pt_adjRTT_test.csv")

both_ds <- rbind(camhs, pt)

date_cols <- c("dob_verified", "act_code_sent_date", "ref_rec_date_opti", 
               "first_treat_app", "ref_date", "ref_rec_date", "app_date", 
               "unav_date_start", "unav_date_end", "header_date")

df_rtt <- both_ds |>
  group_by(!!!syms(data_keys)) |>
  mutate(across(date_cols, ~ as.Date(.x, format = "%d/%m/%Y")),
         
         rtt_unadj = case_when(has_act_code_sent_date == "TRUE" ~ act_code_sent_date - ref_rec_date_opti,
                               TRUE ~ first_treat_app - ref_rec_date_opti),

# make clock_start
# basic rtt - ref_rec_date
# process in sequence:
# DNA/CNA/CNW - app purpose - 2 or 3; att status 8 or 3 or 5; app_date <126 days after ref_rec_date OR prev dna-d app_date; use app_date instead of ref date
# DNA/CNA/CNW after 18wks - app purpose - 2 or 3; att status 8 or 3 or 5; app_date >126 days after ref_rec_date OR prev dna-d app_date; use ref date


# make clock_stop?
# earliest app with app_purpose - 2 or 3 AND att_status = 1 = use app_date
# i.e. first_treat_app

# adjust
# case_when unav_date_start !is.na AND is < ref_rec_date + 126 AND < clock_stop AND unav_start_date < 01/04/2024 AND dataset_type = PT, (clock stop - clock start) - (unav_end - unav_start OR unav_days)
# case_when unav_date_start !is.na AND is < clock_stop AND unav_start_date > 01/04/2024 AND dataset_type = PT, (clock stop - clock start) - (unav_end - unav_start OR unav_days)
# case_when unav_date_start !is.na AND is < ref_rec_date + 126 AND < clock_stop AND dataset_type = CAMHS, (clock stop - clock start) - (unav_end - unav_start OR unav_days)
# case_when unav_start_date is.na AND unav_days !is.na - how do we know if before first treatment app?
        
         clock_start = case_when(app_purpose %in% c(2, 3) &
                                   att_status %in% c(3, 5, 8) &
                                   app_date < (ref_rec_date + 126) ~ app_date, # DNA/CNA/CNW before 18 weeks
                                 
                                TRUE ~ ref_rec_date), # if no special conditions met, use referral date

# this does work but doesn't fill for all clock start entries and doesn't update start date for next DNA
         
          rtt_adj = case_when(dataset_type == "PT" &
                                !is.na(unav_date_start) &
                                unav_date_start < (clock_start + 126) &
                                unav_date_start < clock_start & # this is for PT unavailability before 18 weeks before 1st apr
                                unav_date_start < "01/04/2024" ~ ((first_treat_app - clock_start) - (unav_date_end - unav_date_start)),
                              
                              dataset_type == "PT" &
                                !is.na(unav_date_start) &
                                unav_date_start < clock_start & # this is for PT unavailability after 1st apr
                                unav_date_start > "01/04/2024" ~ ((first_treat_app - clock_start) - (unav_date_end - unav_date_start)),
                              
                              dataset_type == "CAMHS" &
                                !is.na(unav_date_start) &
                                unav_date_start < (clock_start + 126) & # this is for CAMHS unavailability
                                unav_date_start < clock_start ~ ((first_treat_app - clock_start) - (unav_date_end - unav_date_start)),
                              
                              
                              TRUE ~ (first_treat_app - clock_start)
                                ) # ditto - works but does not fill

)
