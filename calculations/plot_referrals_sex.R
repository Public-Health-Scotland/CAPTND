##########################.
###   plot referrals   ###
##########################.



# 1 load libraries --------------------------------------------------------

library(ggplot2)
library(plotly)
library(stringr)


# 2 Function --------------------------------------------------------------

plot_referrals_sex <- function(df_referrals_details, dset){
  
  
  prep_plot <- df_referrals_details %>% 
    mutate(!!sex_reported_o := case_when(
                                         !!sym(sex_reported_o) == 1 ~ 'male',
                                         !!sym(sex_reported_o) == 2 ~ 'female',
                                         TRUE ~ 'indeterminate/intersex/not specified/not recorded')) %>% 
    
    filter(!!sym(dataset_type_o) == dset,
           !!sym(referral_month_o)> (most_recent_month_in_data- months(15))) %>%
    select(!!hb_name_o,!!dataset_type_o,!!referral_month_o,n,n_total,!!ref_acc_o,!!sex_reported_o) %>% 
    group_by(across(all_of(c(hb_name_o,dataset_type_o,referral_month_o,!!sex_reported_o,ref_acc_o)))) %>% 
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
               colour=!!sym(sex_reported_o),
               group=!!sym(sex_reported_o),
               text = paste0(
                 "Health Board: ", hb_name, "<br>",
                 "Sex at birth: ", !!sym(sex_reported_o),"<br>",
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
      #reds
      #'#751A04',
      #blue
      "#0078D4"))+
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b\n%y")+
    labs(title=paste('Referrals accepted by sex at birth in Scotland -',dset),
         colour='Sex at birth', 
         x=time_var,
         y='Percentage of referrals accepted') +
    theme_minimal()+
    theme(legend.position = "top",
          plot.caption = element_text(hjust = 0))+
    theme(legend.position="bottom")+
    theme(plot.title = element_text(hjust = 0.5))+
    facet_wrap(~hb_name)+ #face wrap doesn't work
    theme(panel.spacing = unit(1, "lines"))
  
  
  fig2=ggplotly(p,tooltip = "text")
  
  fname=paste0(referrals_dir,'/referrals_sex_at_birth_',dset,'.html')
  
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








