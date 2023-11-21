##########################.
###   plot referrals   ###
##########################.



# 1 load libraries --------------------------------------------------------

library(ggplot2)
library(plotly)



# 2 Function --------------------------------------------------------------

plot_referrals <- function(df_referrals_details, hb, option_var){
  
  
  option_var=sex_reported_o
  hb='NHS Scotland'
  time_var=referral_month_o
  value_var='n'
  data_name='Referrals'
  
  last
  
  
  prep_plot <- df_referrals_details %>% 
    mutate(!!sex_reported_o := case_when(!!sym(sex_reported_o) == 0 ~ 'indeterminate/intersex',
                                         !!sym(sex_reported_o) == 1 ~ 'male',
                                         !!sym(sex_reported_o) == 2 ~ 'female',
                                         TRUE ~ 'not specified/not recorded')) %>% 
    filter(!!sym(hb_name_o) == hb,
           !!sym(referral_month_o)> (most_recent_month_in_data- months(15))) %>%
    select(!!hb_name_o,!!dataset_type_o,!!referral_month_o,n,!!option_var, !!ref_acc_o) %>% 
    group_by(across(all_of(c(hb_name_o,dataset_type_o,time_var,option_var,ref_acc_o)))) %>% 
    summarise(value=sum(!!sym(value_var)),.groups = 'drop') 
  
  p <- prep_plot %>%  
   ggplot(aes(x=!!sym(time_var),
              y=value, 
              colour=!!sym(ref_acc_o),
              group=!!sym(ref_acc_o),
              text = paste0(
                "Health Board: ", hb_name, "<br>",
                "n: ", value,"<br>",
                option_var,': ',!!sym(option_var))
              )) +
    geom_line()+
    geom_point()+
      scale_colour_manual(values=c(
        #purples
        "#3F3685",
        #blue
        "#0078D4",
        #magenta
        "#9B4393",
        #reds
        '#751A04'))+
      labs(title=paste(hb,option_var,data_name),
           #fill=!!sym(option_var), 
           x='time',
           y='n') +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      theme(legend.position="bottom")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~dataset_type+sex_reported)+
      theme(panel.spacing = unit(1, "lines"))
    
    
    fig2=ggplotly(p,tooltip = "text")
    
    pname=paste0(rtt_dir,'/',data_name,
                 '.html')
    fname=paste0(rtt_dir,'/',data_name,
                 '.csv')
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = pname, #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    
    
    # 
    # message(paste('RTT potential stats on',
    #               data_name,
    #               'can be found on\n',
    #               pname, 'and\n',
    #               fname))
    # 
  
  
}








