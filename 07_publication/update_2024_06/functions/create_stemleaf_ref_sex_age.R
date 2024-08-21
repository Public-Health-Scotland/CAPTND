
##########################################################.
### Create referrals by sex and age stem and leaf plot ###
##########################################################.

# Author: Charlie Smith
# Date: 2024-08-21





create_stemleaf_ref_sex_age <- function(ds){
  
  df_ref_sex_age <- read_parquet(paste0(ref_dir, "referrals_sex_age_", "hb.parquet")) |> 
    ungroup() |> 
    filter(hb_name == "NHS Scotland" &
             dataset_type == ds &
             sex_reported %in% c("Male", "Female") &
             !is.na(age_at_ref_rec)) |> 
    rename(age_ref = age_at_ref_rec, 
           Sex = sex_reported, 
           referrals = count)
  
  if(ds == "CAMHS"){ 
  
    df <- df_ref_sex_age |> 
      mutate(age_label = if_else(age_ref >= 20, "20+", as.character(age_ref))) |> 
      group_by(Sex, age_label) |> 
      summarise(referrals = sum(referrals, ra.rm = TRUE)) |> 
      mutate(age_label = factor(age_label, levels = c(as.character(0:19), "20+"))) |> 
      arrange(Sex, age_label)
  
  } else if(ds == "PT"){
    
    df <- df_ref_sex_age |> 
      mutate(age_label = age_ref)
    
  }
  
  # make chart
  pal <- c(
    "Female" = "#AF69A9", # magenta
    "Male" = "#655E9D" # darkest blue
  )
  
  chart_ref_sco_age_sex <- ggplot(data = df) +
    geom_bar(aes(age_label, referrals, group = Sex, fill = Sex), stat = "identity", width=1, colour="black",
             subset(df, df$Sex == "Female"))+
    geom_bar(aes(age_label, -referrals, group = Sex, fill = Sex), stat = "identity", width=1, colour="black",
             subset(df, df$Sex == "Male"))+
    scale_y_continuous(breaks = seq(plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling)*-1, 
                                    plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling), 
                                    1000),
                       labels = format(abs(seq(plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling)*-1,
                                               plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling),
                                               1000)), big.mark = ","),
                       limits = c(plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling)*-1, 
                                  plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling))
                       )+
    # scale_x_continuous(limits = c(0, plyr::round_any(max(df$age_ref, na.rm = T), 10, f = ceiling)),
    #                    breaks = seq(0, 
    #                                 plyr::round_any(max(df$age_ref, na.rm = T), 100, f = ceiling), 
    #                                 ifelse(ds == "CAMHS", 5, 10)))+
    labs(x = "Age at Referral", 
         y = "Referrals", 
         fill = "Sex", 
         caption = paste0("Source: CAPTND, ", production_month))+ #, 
    #title = "Age distribution of referrals by sex, NHS Scotland* 2020")+
    theme(legend.position = 'right')+
    scale_fill_manual(values = pal)+
    theme_classic()
  
  # save image as PNG
  ggsave(filename = paste0("referrals_sex_age_", ds, ".png"),
         plot = chart_ref_sco_age_sex, 
         device = png,
         path = ref_dir,
         width = chart_width, # UPDATE
         height = chart_height, # UPDATE
         units = "cm",
         dpi = 300,
         bg = "white")
  
}


