
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
apps_att_dir <- paste0(shorewise_pub_data_dir, "/appointments_att/")
basic_opti_dir <- paste0(shorewise_pub_data_dir, "/basic_v_opti/")
markdown_dir <- paste0(root_dir, "/markdown/")


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


# 5. Plotting constants -------------------------------------------------------

# chart dimensions
chart_width = 18 # changed to scale better on a4 page # 24
chart_height = 12 # changed to sclae better o a4 page # 16
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
date_label_yr <- format((as.Date(month_end) %m-% months(12)), "%B %Y")
date_label_5qt <- format((as.Date(month_end) %m-% months(14)), "%B %Y")

# set pub_date as 3 months after month_end
pub_date <- as.Date(month_end %m+% months(3))
## Get next pub date and prev pub date
# (6 months after pub_date, 1st Tuesday)
next_pub_date <- id_next_pub(pub_date)
# (6 months after 1yr before pub_date, 1st Tuesday)
prev_pub_date <- id_next_pub(pub_date - months(12))
