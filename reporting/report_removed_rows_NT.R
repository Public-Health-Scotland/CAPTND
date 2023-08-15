library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(lubridate)
source('config/new_colnames.R')



df <-  list.files(path = "../../../output/removed/", pattern ="swift.*\\.csv$", full.names = TRUE) %>% 
  map_df(~read_csv(.)) %>% 
  group_by(!!sym(hb_name_o),!!sym(dataset_type_o),!!sym(submission_date_o)) %>% 
  bind_rows(summarise(.,
                      across(all_of(c('perc_removed','removed_rows')), sum),
                      across(all_of(c('total_rows')), max),
                      across(where(is.character), ~"removed"),
                      .groups = "drop")) 

#ds='CAMHS'
ds='PT'

df1=df %>% filter(dataset_type==ds) 


timePeriod=1 #time in years that the report will report on. 
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
df1 %>% filter(!!sym(submission_date_o)>(max(!!sym(submission_date_o))- years(timePeriod))) %>% 
  mutate(!!submission_date_o:=ym(format(!!sym(submission_date_o), "%Y-%m"))) %>% 
  ggplot( aes(x=submission_date, y=perc_removed, group=issue, colour=issue)) +
  geom_line()+
  geom_point()+
  theme_minimal()+
  ylab("Removed rows breakdown")+
  xlab("Submission date")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_date(
    minor_breaks = NULL,
    breaks = seq.Date(
      from = min(df1$submission_date),
      to = max(df1$submission_date),
      by = "month"))+
  labs(colour= "Issue")+
  facet_wrap(~factor(hb_name, levels=c(level_order)))+
  theme(plot.margin = unit(c(1,0.5,0.5,0.5), "cm"))+
  theme(legend.position="bottom")

savingLocation <- paste0("../../../output/investigations/", ds,"_removed_rows_breakdown")

ggsave(paste0(savingLocation,
              'plot_',
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



