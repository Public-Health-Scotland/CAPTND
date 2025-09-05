
##########################################################.
### Create referrals by sex and age stem and leaf plot ###
##########################################################.

# Author: Charlie Smith
# Date: 2024-08-21





create_stemleaf_ref_sex_age <- function(ds){
  
  df_ref_sex_age <- read_parquet(paste0(ref_dir, "referrals_sex_age_", "hb.parquet")) |> 
    ungroup() |> 
    filter(!!sym(hb_name_o) == "NHS Scotland" &
             !!sym(dataset_type_o) == ds &
             !!sym(sex_reported_o) %in% c("Male", "Female") &
             !is.na(!!sym(age_at_ref_rec_o))) |> 
    rename(age_ref = !!sym(age_at_ref_rec_o), 
           Sex = !!sym(sex_reported_o), 
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
    "Male" = "#3F3685" # darkest blue
  )


  if(ds == "CAMHS"){ 
    
    brks <- c(as.character(seq(from = 0, to = 18, by = 2)), "20+") 
    
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
       scale_x_discrete(breaks = brks,
                        limits = NULL)+
      labs(x = "Age at Referral", 
           y = "Referrals", 
           fill = "Sex", 
           caption = paste0("CAPTND extract, ", data_analysis_latest_date))+
      theme(legend.position = 'right')+
      scale_fill_manual(values = pal)+
      theme_captnd()
    
  } else if(ds == "PT"){
    
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
                                    plyr::round_any(max(df$referrals, na.rm = T), 1000, f = ceiling)))+
      labs(x = "Age at Referral", 
           y = "Referrals", 
           fill = "Sex", 
           caption = paste0("CAPTND extract, ", data_analysis_latest_date))+
      theme(legend.position = 'right')+
      scale_fill_manual(values = pal)+
      theme_captnd()
    
  } 

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


