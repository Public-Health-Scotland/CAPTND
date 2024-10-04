
library(NHSRplotthedots)

source("./07_publication/script/chapters/2_load_functions.R")
source("./07_publication/script/chapters/3_set_constants.R")

#Adjusted CAPTND graph theme
theme_captnd <- function(){
  font <- "Arial"
  theme_minimal() %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
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


#referrals df
refs_df <- read_parquet(paste0(shorewise_pub_data_dir, "/referrals/referrals_month_hb.parquet"))

#run chart function
rtt_runchart <- function(ds_type, hb){
  
  pat_refs <- refs_df |>
    filter(dataset_type == ds_type,
           hb_name == hb)
  
  pat_refs |>
    ptd_spc(
      value_field = count,
      date_field = referral_month,
      improvement_direction = "neutral"
    ) |>
    ptd_create_ggplot(
      y_axis_label = "Number of patient referrals",
      x_axis_label = "Month of referral",
      main_title = paste0("Number of patients referred to ", ds_type ," services in ", hb),
      colours = ptd_spc_colours(
        value_line = "#9B4393",
        mean_line = "#3F3685",
        lpl = "#0078D4",
        upl = "#0078D4",
        #target = ,
        common_cause = "#9B4393",
        special_cause_neutral = "#3F3685",
      )
    )
  
}

rtt_df <- rtt_runchart('CAMHS', 'NHS Fife')

rtt_df + theme_captnd() +
  theme(panel.grid.major.y = element_line(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.box.spacing = unit(0, "cm"),
        legend.key.height = unit(10, "pt"),
        legend.key.width = unit(30, "pt"),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1))

