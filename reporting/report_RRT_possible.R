library(arrow)
library(dplyr)
library(ggplot2)
library(plotly)
library(phsstyles)
library(readr)
source('config/new_colnames.R')


#df_complete=read_parquet('../../../output/df_glob_swift_completed_2023-08-25.parquet')


report_RTT_cols_completion <- funcion(df){

  df_eval=df_complete %>% 
    group_by(across(all_of(data_keys))) %>% 
    mutate(rtt_eval=case_when(
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'complete rtt',
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'no app purpose',
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no app status',
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no ref acc',
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(ref_rej_date)) ~ 'ref rej',
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(act_code_sent_date_o))) ~ 'online treatment',
      any((is.na(!!sym(ref_rec_date_o)) & is.na(!!sym(ref_date_o)))& 
            is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no ref info',
      any((!is.na(!!sym(ref_rec_date_o))|!is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))) ~ 'patient waiting',
      any((is.na(!!sym(ref_rec_date_o)) & is.na(!!sym(ref_date_o)))& 
            !is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            !is.na(!!sym(att_status_o))& 
            !is.na(!!sym(app_purpose_o))) ~ 'no ref date',
      any(!is.na(!!sym(case_closed_date_o))) ~ 'case closed',
      any(is.na(!!sym(ref_rec_date_o)) & 
            is.na(!!sym(ref_date_o))& 
            is.na(!!sym(ref_acc_o))& 
            is.na(!!sym(app_date_o))&
            is.na(!!sym(att_status_o))& 
            is.na(!!sym(app_purpose_o))&
            (!is.na(diag_1)|!is.na(treat_1))) ~ 'no app - treat/diag',
      any(is.na(!!sym(ref_rec_date_o))&
            is.na(!!sym(ref_date_o))& 
            is.na(!!sym(ref_acc_o))& 
            !is.na(!!sym(app_date_o))&
            (is.na(!!sym(att_status_o))| 
            is.na(!!sym(app_purpose_o)))) ~ 'no ref and app details'),
  .after=!!chi_valid_o) %>% 
  ungroup()

  df_sub_system=read_csv('../../../data/hb_sub_system.csv')
  
  df_stats=df_eval %>% 
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
    inner_join(df_sub_system, by=c(hb_name_o, dataset_type_o), relationship='many-to-many')
  
  
  barsPlt_prep = df_stats %>%
    mutate(rtt_eval=factor(rtt_eval, level = c('complete rtt','patient waiting','ref rej',
                                               'online treatment','case closed',
                                               "no ref info",'no ref date', "no ref acc",
                                               'no ref and app details',
                                               'no app purpose',"no app status",
                                               'no app - treat/diag'))) %>% 
    ggplot(aes(factor(hb_name, level = level_order), 
               perc_pathways, 
               fill=rtt_eval, 
               group=rtt_eval,
               text = paste0(
                 "Health Board: ", hb_name, "<br>",
                 "RTT eval: ", rtt_eval, "<br>",
                 "% pathways: ", perc_pathways, "<br>",
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
                               "#0078D4",
                               #magenta
                               "#9B4393",
                               "#AF69A9",
                               #"#CDA1C9",
                               #reds
                               "#C73918",
                               "#D26146",
                               "#E39C8C",
                               "#EEC4BA",
                               "#F9EBE8",
                               "#F9EBE8",
                               "#E1C7DF"))+
    labs(title=paste0("Percentage of pathways where RTT is possible"),
         fill='RTT eval', 
         x='health board',
         y='% pathways') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          plot.caption = element_text(hjust = 0))+
    theme(legend.position="bottom")+
    theme(plot.title = element_text(hjust = 0.5))
  
  p1=p+
    facet_wrap(~dataset_type+sub_system)+
    theme(panel.spacing = unit(1, "lines"))
  
  p2=p+
    facet_wrap(~dataset_type)+
    theme(panel.spacing = unit(1, "lines"))
  
  fig1=ggplotly(p1,tooltip = "text")
  
  fig2=ggplotly(p2,tooltip = "text")
  
  htmlwidgets::saveWidget(
    widget = fig1, #the plotly object
    file = paste0('../../../output/investigations/RTT_plot_detailed_PMS.html'), #the path & file name
    selfcontained = TRUE #creates a single html file
  )
  htmlwidgets::saveWidget(
    widget = fig2, #the plotly object
    file = paste0('../../../output/investigations/RTT_plot_detailed.html'), #the path & file name
    selfcontained = TRUE #creates a single html file
  )
  
 
}

