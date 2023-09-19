library(arrow)
library(dplyr)
library(ggplot2)
library(plotly)
library(phsstyles)
library(readr)
source('config/new_colnames.R')


#df=read_parquet('../../../output/df_glob_swift_completed_2023-08-25.parquet')


report_RTT_cols_completion <- function(df, dateForFile){

  df_eval=df_glob_swift_completed %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(rtt_eval=case_when(
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'complete rtt',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'no app purpose',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no app status',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'no accept/purpose',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'no status/purpose',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no ref acc',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(ref_rej_date)) ~ 'ref rej',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(act_code_sent_date_o))) ~ 'online treatment',
      any(is.na(!!sym(ref_rec_date_opti_o))& 
            is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no ref info',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'patient waiting',
      any(!is.na(!!sym(ref_rec_date_opti_o))& 
            is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'referral pending',
      any(is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no ref date',
      #any(!is.na(!!sym(case_closed_date_o))) ~ 'case closed',
      #not a useful measure
      any(is.na(!!sym(ref_rec_date_opti_o))& 
            is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))&
            (!is.na(!!sym(diag_1_o))|
               !is.na(!!sym(treat_1_o))|
               !is.na(!!sym(measure_1_o)))) ~ 'no ref/app - only treat/diag/outc',
      any(is.na(!!sym(ref_rec_date_opti_o))& 
            #is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            (is.na(!!sym(att_status_o))| 
            is.na(!!sym(app_purpose_o)))) ~ 'no ref/app details',
      any(is.na(!!sym(ref_rec_date_opti_o))& 
            !is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))&
            is.na(!!sym(app_purpose_o))) ~ 'only ref acc',
      any(is.na(!!sym(ref_rec_date_opti_o))& 
            is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'missing everything'
      ),
  .after=!!chi_valid_o) %>% 
  ungroup()

  
  sub_source_ev= df %>% 
    select(all_of(data_keys),!!sub_source_o) %>% 
    distinct() %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(n=n(),
           sub_source_eval=case_when(n==1 & sub_source=='swift' ~ 'swift',
                                     n==1 & sub_source=='globalscape' ~ 'globalscape',
                                     n>1 ~ 'both')) %>% 
    select(-c(!!sub_source_o,n)) %>% 
    distinct() %>% 
    ungroup()
  
  df_eval=df_eval %>% 
    inner_join(sub_source_ev, by=data_keys)
  
  df_sub_system=read_csv_arrow('../../../data/hb_sub_system.csv')
  
  plot_data <- function(data_name, df_eval_filt) {
    df_stats=df_eval_filt %>% 
      select(all_of(data_keys),rtt_eval) %>% 
      distinct() %>% 
      group_by(!!sym(dataset_type_o),!!sym(hb_name_o),rtt_eval) %>% 
      summarise(n=n(), .groups = 'drop') %>% 
      group_by(!!sym(dataset_type_o), rtt_eval) %>% 
      bind_rows(summarise(.,
                          across(where(is.numeric), sum),
                          across(where(is.character), ~"NHS Scotland"),
                          .groups = "drop")) %>% 
      group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
      mutate(total_pathways=sum(n)) %>% 
      group_by(rtt_eval) %>% 
      mutate(perc_pathways=round(n*100/total_pathways,2)) %>% 
      ungroup() %>% 
      left_join(df_sub_system, by=c(hb_name_o, dataset_type_o), relationship='many-to-many')
    
    
    barsPlt_prep = df_stats %>%
      mutate(rtt_eval=factor(rtt_eval, level = c('complete rtt',#1
                                                 'patient waiting',#2
                                                 'ref rej',#3
                                                 'online treatment',#4
                                                 #'case closed',#5
                                                 "no ref info",#6
                                                 'no ref date', #7
                                                 "no ref acc",#8
                                                 'referral pending',#9
                                                 'missing everything',#10
                                                 'no ref/app details',#11
                                                 'no app purpose',"no app status",#12
                                                 'no status/purpose',#13
                                                 'no accept/purpose',#14
                                                 'no ref/app - only treat/diag/outc',
                                                 'only ref acc'))) %>% #15
      ggplot(aes(factor(hb_name, level = level_order), 
                 perc_pathways, 
                 fill=rtt_eval, 
                 group=rtt_eval,
                 text = paste0(
                   "Health Board: ", hb_name, "<br>",
                   "RTT eval: ", rtt_eval, "<br>",
                   "% pathways: ", perc_pathways, "<br>",
                   "n pathways: ", n,"<br>",
                   'PMS: ',sub_system)
      )) 
    
    
    
    p <- barsPlt_prep +
      geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
      #scale_fill_discrete_phs()+
      scale_fill_manual(values=c(#purples
                                 "#3F3685",
                                 "#655E9D",
                                 "#9F9BC2",
                                 "#C5C3DA",
                                 #"#ECEBF3",
                                 #blue
                                 #"#0078D4",
                                 #magenta
                                 "#9B4393",
                                 "#AF69A9",
                                 "#CDA1C9",
                                 #reds
                                 '#751A04',
                                 '#902004',
                                 "#C73918",
                                 "#D26146",
                                 "#E39C8C",
                                 "#EEC4BA",
                                 '#FFE8E2',
                                 "#F9EBE8"
                                 ))+
      labs(title=paste0("Percentage of pathways where RTT is possible - ",data_name),
           fill='RTT eval', 
           x='health board',
           y='% pathways') +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "top",
            plot.caption = element_text(hjust = 0))+
      theme(legend.position="bottom")+
      theme(plot.title = element_text(hjust = 0.5))
    
  
    p2=p+
      facet_wrap(~dataset_type)+
      theme(panel.spacing = unit(1, "lines"))
    
  
    fig2=ggplotly(p2,tooltip = "text")
    
    pname=paste0('../../../output/investigations/RTT_plot_',data_name,'_',
                 as.character(dateForFile),
                 '.html')
    fname=paste0('../../../output/evaluated/RRT_possible_',data_name,'_',
                 as.character(dateForFile),
                 '.csv')
    
    htmlwidgets::saveWidget(
      widget = fig2, #the plotly object
      file = pname, #the path & file name
      selfcontained = TRUE #creates a single html file
    )
    
    write_csv_arrow(df_stats, fname)
 
  
  message(paste('RTT potential stats on',
                data_name,
                'can be found on\n',
                pname, 'and\n',
                fname))
  }
  
  
  plot_data('all cases',df_eval)
  plot_data('closed cases',df_eval %>% filter(!is.na(!!sym(case_closed_date_o))))
  plot_data('open cases',df_eval %>% filter(is.na(!!sym(case_closed_date_o))))
  
  plot_data('all cases - swift',df_eval %>% filter(sub_source_eval=='swift'))
  plot_data('closed cases - swift',df_eval %>% filter(sub_source_eval=='swift' & !is.na(!!sym(case_closed_date_o))))
  plot_data('open cases - swift',df_eval %>% filter(sub_source_eval=='swift' & is.na(!!sym(case_closed_date_o))))
  
  plot_data('all cases - swift and both',df_eval %>% filter(sub_source_eval!='globalscape'))
  plot_data('closed cases - swift and both',df_eval %>% filter(sub_source_eval!='globalscape' & !is.na(!!sym(case_closed_date_o))))
  plot_data('open cases - swift and both',df_eval %>% filter(sub_source_eval!='globalscape' & is.na(!!sym(case_closed_date_o))))
  
  plot_data('all cases - globalscape',df_eval %>% filter(sub_source_eval=='globalscape'))
  plot_data('closed cases - globalscape',df_eval %>% filter(sub_source_eval=='globalscape' & !is.na(!!sym(case_closed_date_o))))
  plot_data('open cases - globalscape',df_eval %>% filter(sub_source_eval=='globalscape' & is.na(!!sym(case_closed_date_o))))
  
  return(df_eval)
}

