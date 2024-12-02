### Pull together excel summaries of new demographic items for publication scoping ####

#Bex Madden
#29/11/2024


# Read in pre-made all time and quarterly breakdows of each item
df1 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/referrals_ethnicity_all.parquet"))
df2 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/referrals_ethnicity_grp_all.parquet"))
df3 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/referrals_ethnicity_grp_all_hb.parquet"))
df4 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ethnicity/referrals_ethnicity_grp_all_qt.parquet"))

df5 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_lac/referrals_lac_all_hb.parquet"))
df6 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_lac/referrals_lac_qt_hb.parquet"))

df7 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/referrals_prot_adult_all_hb.parquet"))
df8 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/referrals_prot_adult_qr_hb.parquet"))
df9 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/referrals_prot_child_all_hb.parquet"))
df10 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_prot_status/referrals_prot_child_qr_hb.parquet"))

df11 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/referrals_ppmh_all_hb_sex.parquet"))
df12 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_ppmh/referrals_ppmh_qt_hb_sex.parquet"))

df13 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_vet_status/referrals_vets_all_hb.parquet"))
df14 <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals_by_vet_status/referrals_vets_qr_hb.parquet"))


# name tabs
list_tabs <- list(
  
  eth_all_alltime = df1,
  eth_grp_alltime = df2,
  eth_grp_alltime_hb = df3,
  eth_grp_qt = df4,
  
  lac_alltime_hb = df5,
  lac_qt_hb = df6,
  
  prot_adult_alltime_hb = df7,
  prot_adult_qt_hb = df8,
  prot_child_alltime_hb = df9,
  prot_child_qt_hb = df10,
  
  preg_alltime_hb_sex = df11,
  preg_qt_hb_sex = df12,
  
  vet_alltime_hb = df13,
  vet_qt_hb = df14)

# save output as excel doc
filepath = paste0(shorewise_pub_measure_summaries_dir, "/ref_demographics_summaries_nov24.xlsx")
export(list_tabs, file = filepath)

# format report
wb <- loadWorkbook(filepath)

for(i in 1:length(list_tabs)){
  setColWidths(wb, sheet = i, cols = 1:9, widths = "auto")
}

saveWorkbook(wb, filepath, overwrite =TRUE)
