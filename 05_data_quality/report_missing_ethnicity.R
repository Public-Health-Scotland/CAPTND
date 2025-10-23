####################################################
### Check missing ethnicity status of referrals  ###
####################################################

# month_start <- as.Date('2025-06-01')
# month_end <- as.Date('2025-06-30')
#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#Highlights referrals received in the latest month with either a missing ethnicity status
#or an ethnicity status of unknown (99)

missing_ethnicity <- function(){
  
  missing_ethnicity_df <- df |>
    filter(!is.na(!!sym(ref_date_o)) | !is.na(!!sym(ref_rec_date_o)),
           is.na(!!sym(act_code_sent_date_o)),
           !!sym(ref_acc_o) == '01',
           !is.na(!!sym(ucpn_o)) & !!sym(ucpn_o) != "0" & !!sym(ucpn_o) != "NULL",
           !is.na(!!sym(chi_o)) & !!sym(chi_o) != "0" & !!sym(chi_o) != "NULL",
           !!sym(header_date_o) == month_start,
           is.na(!!sym(ethnicity_o)) | !!sym(ethnicity_o) == '99') |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(ref_rec_date_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o),
           !!sym(ref_rec_date_o), !!sym(ref_date_o), !!sym(ethnicity_o)) |>
    write_parquet(paste0(stats_checked_dir, "/missing_ethnicity_", month_start, ".parquet"))
  
}

