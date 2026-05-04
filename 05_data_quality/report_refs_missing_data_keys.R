################################################
### Check missing UCPN and CHI of referrals  ###
################################################

# month_start <- as.Date('2026-03-01')
# month_end <- as.Date('2026-03-31')
#df <- read_parquet(paste0(root_dir, "/swift_extract.parquet")) 

#Highlights referrals received in the latest month with either a missing ucpn or CHI

missing_ucpn_chi <- function(){
  
  missing_ucpn_chi_df <- df |>
    filter(!is.na(!!sym(ref_date_o)) | !is.na(!!sym(ref_rec_date_o)),
           !!sym(header_date_o) == month_start,
           is.na(!!sym(ucpn_o)) | is.na(!!sym(chi_o))) |>
    arrange(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ref_rec_date_o)) |>
    select(!!sym(dataset_type_o), !!sym(hb_name_o), !!sym(ucpn_o), !!sym(chi_o), !!sym(upi_o),
           !!sym(ref_rec_date_o), !!sym(ref_date_o), !!sym(header_date_o)) |> 
    mutate(hb_name = case_when(hb_name == 'NHS Lanarkshire' & nchar(ucpn) == 9 ~ 'NHS Greater Glasgow and Clyde',
                               TRUE ~ hb_name)) |>
    write_parquet(paste0(stats_checked_dir, "/missing_ucpn_chi_", month_start, ".parquet"))
  
}



