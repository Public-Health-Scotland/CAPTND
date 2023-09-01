library(arrow)
library(dplyr)
library(ggplot2)
library(plotly)
source('config/new_colnames.R')


report_RTT_cols_completion <- funcion(df){
  
  
  all_pathways=df_complete %>% 
    select(all_of(data_keys)) %>% 
    distinct()
  
  df_completed_rtt = df_complete %>% 
    filter(if_all(all_of(vec_rtt_unadj_cols), ~ !is.na(.)))
  
  
  
  
}



df_complete=read_parquet('../../../output/df_glob_swift_completed_2023-08-25.parquet')

df_complete_all_pathways=df_complete %>% 
  select(all_of(data_keys)) %>% 
  distinct()

df_completed_rtt = df_complete %>% 
  filter(if_all(all_of(vec_rtt_unadj_cols), ~ !is.na(.)))

df_completed_rtt_pathways=df_completed_rtt %>% 
  select(all_of(data_keys)) %>% 
  distinct()

df_completed_rtt_pathways_summary=df_completed_rtt_pathways %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
  summarise(pathways_rtt=n(),.groups = 'drop') %>% 
  mutate(rrt_possible=TRUE)

df_complete_all_pathways_summary=df_complete_all_pathways %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
  summarise(pathways_rtt=n(),.groups = 'drop') %>% 
  mutate(rrt_possible=FALSE)

df_stats=bind_rows(df_completed_rtt_pathways_summary,
                   df_complete_all_pathways_summary) %>% 
  group_by(!!sym(dataset_type_o), rrt_possible) %>% 
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"NHS Scotland"),
                      .groups = "drop")) %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o)) %>% 
  mutate(total_pathways=sum(pathways_rtt)) %>% 
  group_by(rrt_possible) %>% 
  mutate(perc_pathways=round(pathways_rtt*100/total_pathways,2)) %>% 
  ungroup()




barsPlt_prep = df_stats %>% 
  ggplot(aes(factor(hb_name, level = level_order), 
             perc_pathways, 
             fill=rrt_possible, 
             group=rrt_possible,
             text = paste0(
               "Health Board: ", hb_name, "<br>",
               "RRT possible: ", rrt_possible, "<br>",
               "% pathways: ", perc_pathways)
  )) 



p <- barsPlt_prep +
  geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
  scale_fill_manual(values=c(#"#3F3685",
                             "#9B4393",
                             #"#0078D4",
                             "#83BB26"))+
  labs(title=paste0("Number of pathways where RTT is possible"),
       fill='RTT possible', 
       x='health board',
       y='% pathways') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        plot.caption = element_text(hjust = 0))+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~dataset_type)+
  theme(panel.spacing = unit(1, "lines"))

fig2=ggplotly(p,tooltip = "text")

htmlwidgets::saveWidget(
  widget = fig2, #the plotly object
  file = paste0('../../../output/investigations/RTT_plot.html'), #the path & file name
  selfcontained = TRUE #creates a single html file
)




