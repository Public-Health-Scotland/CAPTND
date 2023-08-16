library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(lubridate)
library(plotly)
library(htmlwidgets)
source('config/new_colnames.R')



df <-  list.files(path = "../../../output/removed/", pattern ="swift.*\\.csv$", full.names = TRUE) %>% 
  map_df(~read_csv(.)) %>% 
  # group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
  # bind_rows(summarise(.,
  #                     across(all_of(c('perc_removed','removed_rows')), sum),
  #                     across(all_of(c('total_rows')), max),
  #                     across(where(is.character), ~"removed"),
  #                     .groups = "drop")) %>% 
  mutate(fin_year=extract_fin_year(!!sym(submission_date_o)),
         interval='monthly') %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o), fin_year,issue) %>% 
  bind_rows(summarise(.,
                      across(all_of(c('removed_rows','total_rows')), sum),
                      across(all_of(c('interval')), ~'yearly'),
                      across(all_of(c('perc_removed')), ~(removed_rows*100)/total_rows),
                      .groups = "drop")) %>% 
  mutate(across(where(is.numeric), round,2))
  
  

#ds='CAMHS'
ds='PT'

df1=df %>% filter(dataset_type==ds) 


timePeriod=1 #time in years that the report will report on. 
minDate=max(df$submission_date, na.rm = T)- years(timePeriod)
level_order <- c('NHS Ayrshire and Arran',
                 'NHS Borders',
                 'NHS Dumfries and Galloway',
                 'NHS Fife',
                 'NHS Forth Valley',
                 'NHS Grampian',
                 'NHS Greater Glasgow and Clyde',
                 'NHS Highland',
                 'NHS Lanarkshire',
                 'NHS Lothian',
                 'NHS Orkney',
                 'NHS Shetland',
                 'NHS Tayside',
                 'NHS Western Isles',
                 'NHS24',
                 'NHS Scotland')
p1 <- df1 %>% filter(!!sym(submission_date_o)>ymd(minDate)) %>% 
  mutate(!!submission_date_o:=ym(format(!!sym(submission_date_o), "%Y-%m"))) %>% 
  ggplot( aes(x=submission_date, y=perc_removed, group=issue, colour=issue)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  scale_colour_manual(values=c("#3F3685",
                             "#9B4393",
                             "#0078D4",
                             "#83BB26"))+
  ylab("percentage of rows removed")+
  xlab("Submission date")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(
    minor_breaks = NULL,
    breaks = seq.Date(
      from = min(df1$submission_date, na.rm = T),
      to = max(df1$submission_date, na.rm = T),
      by = "month"))+
  labs(title=paste0("Percentage of removed rows in ",ds," data cleaning by submission month"),
       colour= "Reason for removal")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~factor(hb_name, levels=c(level_order)))+
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
  theme(legend.position="bottom")+
  theme(panel.spacing = unit(1, "lines"))

fig1=ggplotly(p1)

savingLocation <- paste0("../../../output/investigations/", ds,"_removed_rows_breakdown")


htmlwidgets::saveWidget(
  widget = fig1, #the plotly object
  file = paste0(savingLocation,
                'plot_',
                'trend',
                as.character(today()),
                ".html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)


ggsave(paste0(savingLocation,
              'plot_',
              'time_trend',
              as.character(today()),
              ".png"),
       width = 27,
       height = 20,
       units = c("cm"),
       dpi = 300,
       bg='white')


write_csv(df1, paste0(savingLocation,
                           "table_",
                           as.character(today()),
                           ".csv"))


  
  
  
 
barsPlt_prep = df1 %>% filter(interval=='yearly') %>% 
  ggplot(aes(factor(hb_name, level = level_order), perc_removed, fill=issue, group=issue)) 


  
p2 <- barsPlt_prep +
  geom_bar(position=position_stack(reverse = TRUE), stat="identity")+
  scale_fill_manual(values=c("#3F3685",
                             "#9B4393",
                             "#0078D4",
                             "#83BB26"))+
  labs(title=paste0("Percentage of removed rows in ",ds," data cleaning by fiscal year"),
       fill='Reason for removal', 
       x='health board',
       y='percentage of rows removed') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        plot.caption = element_text(hjust = 0))+
  theme(legend.position="bottom")+
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~fin_year)+
  theme(panel.spacing = unit(1, "lines"))

fig2=ggplotly(p2)

htmlwidgets::saveWidget(
  widget = fig2, #the plotly object
  file = paste0(savingLocation,
                'plot_',
                'bar',
                as.character(today()),
                ".html"), #the path & file name
  selfcontained = TRUE #creates a single html file
)
  
ggsave(paste0(savingLocation,
              'plot_',
              'bar',
              as.character(today()),
              ".png"),
       width = 27,
       height = 20,
       units = c("cm"),
       dpi = 300,
       bg='white')


