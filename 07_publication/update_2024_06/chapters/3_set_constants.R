
#####################.
### Set constants ###
#####################.

# Author: Charlie Smith
# Date: 2023-11-14




# 1 - Establish data time frame -------------------------------------------

month_end <- ymd(month_end)
month_start <- ymd(month_end) - months(14)
date_range <- seq.Date(from = month_start, to = month_end, by = "month")

range_12_month <- seq.Date(from = month_end - months(11), to = month_end, by = "month")

# 2 - Production data -----------------------------------------------------
publication_month <- month_end + months(3)


production_date <- Sys.Date()
production_month <- format(as.Date(production_date), "%B %Y")

# 3 - Set safe place to save working data ---------------------------------

ref_dir <- paste0(shorewise_pub_data_dir, "/referrals/")
ref_source_dir <- paste0(shorewise_pub_data_dir, "/referrals_by_ref_source/")
non_acc_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance/")
non_acc_reason_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance_reason/")
non_acc_action_dir <- paste0(shorewise_pub_data_dir, "/non_acceptance_action/")



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

# create complete ds 
df_ds_hb_name <- cross_join(as.data.frame(vec_dataset_type), 
                            as.data.frame(hb_vector)) |> 
  rename(dataset_type = vec_dataset_type,
         hb_name = hb_vector) |> 
  filter(!(#dataset_type == "CAMHS" & 
             hb_name == "NHS 24")) # remove invalid combo


# chart dimensions
chart_width = 24
chart_height = 16
bar_width = 0.5
  
  

# 6 - Get next pub date ---------------------------------------------------
# (6 months after month_end, 1st Tuesday)
# next_pub_date <- id_next_pub(data_last = month_end)
