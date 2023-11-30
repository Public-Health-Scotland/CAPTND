##########################.
###   plot referrals   ###
##########################.



# 1 load libraries --------------------------------------------------------

library(ggplot2)
library(plotly)
library(stringr)


# 2 Function --------------------------------------------------------------

plot_referrals <- function(df_referrals_details, hb, option_var, time_var, value_type){
  
  
   option_var=simd_quintile_o

  hb='NHS Scotland'
  time_var='month'
   value_type='n_perc'
  
  if(value_type=='n'){
    value_type_text='number'
    value_var='value'
    remove_var='value_perc'
  }else{
    value_type_text='percentage'
    remove_var='value'
    value_var='value_perc'
  }
  
  option_var_text = gsub('_', ' ', option_var)
  
  
  prep_plot <- df_referrals_details %>% 
    filter(!is.na(!!sym(option_var))) %>% 
    mutate(!!sex_reported_o := case_when(!!sym(sex_reported_o) == 0 ~ 'indeterminate/intersex',
                                         !!sym(sex_reported_o) == 1 ~ 'male',
                                         !!sym(sex_reported_o) == 2 ~ 'female',
                                         TRUE ~ 'not specified/not recorded'),
           plot_var = paste(!!sym(ref_acc_o), option_var_text, !!sym(option_var))) %>% 
    
    filter(!!sym(hb_name_o) == hb,
           !!sym(referral_month_o)> (most_recent_month_in_data- months(15))) %>%
    select(!!hb_name_o,!!dataset_type_o,!!referral_month_o,n,n_total,!!option_var, !!ref_acc_o,plot_var) %>% 
    group_by(across(all_of(c(hb_name_o,dataset_type_o,referral_month_o,option_var,ref_acc_o)))) %>% 
    mutate(value=sum(n),
              value_total=sum(n_total)) %>% 
    ungroup() %>% 
    mutate(value_perc=round(value*100/value_total,2)) %>% 
    select(-c(!!remove_var, value_total,!!option_var, !!ref_acc_o)) %>% 
      distinct()
    
  #add number AND percentages
  p <- prep_plot %>%  
   ggplot(aes(x=!!sym(referral_month_o),
              y=!!sym(value_var), 
              colour=plot_var,
              group=plot_var,
              text = paste0(
                "Health Board: ", hb_name, "<br>",
                str_to_sentence(value_type_text),": ", !!sym(value_var),"<br>",
                "Group type: ",plot_var,"<br>",
                "Month: ", !!sym(referral_month_o), "<br>"#,
                #str_to_sentence(option_var_text),': ',!!sym(option_var)
                )
              )) +
    geom_line()+
    geom_point()+
      # scale_colour_manual(values=c(
      #   #purples
      #   "#3F3685",
      #   #magenta
      #   "#9B4393",
      #   #reds
      #   '#751A04',
      #   #blue
      #   "#0078D4"))+
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b\n%y")+
      labs(title=paste('Referrals',value_type_text,'in',hb,'divided by',option_var_text),
           colour='Referrals type', 
           x=time_var,
           y='number of referrals') +
      theme_minimal()+
      theme(legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      theme(legend.position="bottom")+
      theme(plot.title = element_text(hjust = 0.5))+
      facet_wrap(~dataset_type)+ #face wrap doesn't work
      theme(panel.spacing = unit(1, "lines"))
    
    
    fig2=ggplotly(p,tooltip = "text")
    
    fname=paste0(referrals_dir,'/referrals_',value_type_text,'_',hb,'_',option_var,'.html')
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = fname, #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    



    message(paste('Referrals',
                  value_type_text,
                  'plot for',
                  hb,
                  'divided by',
                  option_var_text,
                  'can be found on\n',
                  fname))


  
}








