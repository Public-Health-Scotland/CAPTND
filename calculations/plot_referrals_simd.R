##########################.
###   plot referrals   ###
##########################.



# 1 load libraries --------------------------------------------------------

library(ggplot2)
library(plotly)
library(stringr)


# 2 Function --------------------------------------------------------------

plot_referrals_simd <- function(df_referrals_details, dset){
  
  
  prep_plot <- df_referrals_details %>% 
    mutate( !!simd_quintile_o := as.character(!!sym(simd_quintile_o))) %>% 
    filter(!is.na(!!sym(simd_quintile_o)),
           !!sym(dataset_type_o) == dset,
           !!sym(referral_month_o)> (most_recent_month_in_data- months(15))) %>%
    select(!!hb_name_o,!!dataset_type_o,!!referral_month_o,n,n_total,!!ref_acc_o,!!simd_quintile_o) %>% 
    group_by(across(all_of(c(hb_name_o,dataset_type_o,referral_month_o,!!simd_quintile_o,ref_acc_o)))) %>% 
    mutate(value=sum(n),
           value_total=sum(n_total)) %>% 
    ungroup() %>% 
    mutate(value_perc=round(value*100/value_total,2)) %>% 
    select(-c(n, n_total)) %>% 
    distinct() %>% 
    filter(!!sym(ref_acc_o)=='accepted')
  
  #add number AND percentages
  p <- prep_plot %>%  
    ggplot(aes(x=!!sym(referral_month_o),
               y=value_perc, 
               colour=!!sym(simd_quintile_o),
               group=!!sym(simd_quintile_o),
               text = paste0(
                 "Health Board: ", hb_name, "<br>",
                 "SIMD quintile: ", !!sym(simd_quintile_o),"<br>",
                 "Month: ", !!sym(referral_month_o), "<br>",
                 "Ref accepted (%): ",value_perc,"%<br>",
                 "Ref accepted (n): ",value,"<br>",
                 "Total referrals (n): ",value_total,"<br>"
               )
    )) +
    geom_line()+
    geom_point()+
    scale_colour_manual(values=c(
      #purples
      "#3F3685",
      #magenta
      "#9B4393",
      #green
      '#83BB26',
      #blue
      "#0078D4",
      #teal
      '#1E7F84'))+
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b\n%y")+
    labs(title=paste('Referrals accepted by SIMD 2020 quintile in Scotland -',dset),
         colour='SIMD 2020 quintile', 
         x=time_var,
         y='Percentage of referrals accepted') +
    theme_minimal()+
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0))+
    theme(legend.position="bottom")+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~factor(hb_name, levels=c(level_order)))+ #face wrap doesn't work
    theme(panel.spacing = unit(1, "lines"))
  
  
  fig2=ggplotly(p,tooltip = "text")
  
  fname=paste0(referrals_dir,'/referrals_SIMD_quintile_',dset,'.html')
  
  htmlwidgets::saveWidget(
    widget = fig2, #the plotly object
    file = fname, #the path & file name
    selfcontained = TRUE #creates a single html file
  )
  
  
  
  
  message(paste('Referrals by sex at birth -',
                dset,
                'plot can be found on\n',
                fname))
  
  
  
}








