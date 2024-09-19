
############################################################.
### Create chart - referrals distribution by sex and age ###
############################################################.

# Author: Charlie Smith
# Date: 2023-11-17


make_chart_sex_age <- function(dataset_choice = c("CAMHS", "PT")){
  
  # load data 
  df_sex_age <- read_parquet(file = paste0(data_working_safe, "refs_sex_age.parquet")) |> 
    filter(sex %in% c("Male", "Female"), 
           dataset_type == dataset_choice,
           case_when(
             dataset_type == "CAMHS" ~ age_at_ref_rec <= 18,
             TRUE ~ age_at_ref_rec == age_at_ref_rec)) |> 
    dplyr::rename(age_ref = age_at_ref_rec,
                  Sex = sex) 
  
  
  
  # make chart
  pal <- c(
    "Female" = "#AF69A9", # magenta
    "Male" = "#655E9D" # darkest blue
  )
  
  chart_ref_sco_age_sex <- ggplot(data = df_sex_age) +
    geom_bar(aes(age_ref, referrals, group = Sex, fill = Sex), stat = "identity", width=1, colour="black",
             subset(df_sex_age, df_sex_age$Sex == "Female"))+
    geom_bar(aes(age_ref, -referrals, group = Sex, fill = Sex), stat = "identity", width=1, colour="black",
             subset(df_sex_age, df_sex_age$Sex == "Male"))+
    scale_y_continuous(breaks = seq(plyr::round_any(max(df_sex_age$referrals, na.rm = T), 1000, f = ceiling)*-1, 
                                    plyr::round_any(max(df_sex_age$referrals, na.rm = T), 1000, f = ceiling), 
                                    1000),
                       labels = format(abs(seq(plyr::round_any(max(df_sex_age$referrals, na.rm = T), 1000, f = ceiling)*-1,
                                               plyr::round_any(max(df_sex_age$referrals, na.rm = T), 1000, f = ceiling),
                                               1000)), big.mark = ","),
                       limits = c(plyr::round_any(max(df_sex_age$referrals, na.rm = T), 1000, f = ceiling)*-1, 
                                  plyr::round_any(max(df_sex_age$referrals, na.rm = T), 1000, f = ceiling)))+
    scale_x_continuous(limits = c(0, plyr::round_any(max(df_sex_age$age_ref, na.rm = T), 10, f = ceiling)),
                       breaks = seq(0, 
                                    plyr::round_any(max(df_sex_age$age_ref, na.rm = T), 100, f = ceiling), 
                                    ifelse(dataset_choice == "CAMHS", 5, 10)))+
    labs(x = "Age at Referral", 
         y = "Referrals", 
         fill = "Sex", 
         caption = paste0("Source: CAPTND, ", production_month))+ #, 
    #title = "Age distribution of referrals by sex, NHS Scotland* 2020")+
    theme(legend.position = 'right')+
    scale_fill_manual(values = pal)+
    theme_classic()
  
  # save image as PNG
  ggsave(filename = paste0("chart_sex_age_", dataset_choice, ".png"),
         plot = chart_ref_sco_age_sex, 
         device = png,
         path = data_working_safe,
         width = chart_width, 
         height = chart_height,
         units = "cm",
         dpi = 300,
         bg = "white")
  
  
}
