camhs_weird_dob=df_swift_clean %>% filter(dob<today(),dataset_type=='CAMHS')

camhs_weird_dob_boards = df_swift_clean %>% 
  filter(dob<ymd(20000101),dataset_type=='CAMHS') %>% 
  select(hb_name, dataset_type, chi, dob) %>% 
  distinct()


camhs_weird_dob_boards_list <- camhs_weird_dob_boards %>% 
  group_by(hb_name) %>% 
  group_split()

for(x in camhs_weird_dob_boards_list){
  nameOfBoard=unique(x$hb_name)
  write_csv(x, paste0("../../../output/investigations/tooOldForCAMHS/tooOldForCAMHS_",nameOfBoard ,".csv"))
}


tay=df_swift_clean %>% filter(chi %in% c('0111970547',
                                     '1110970226',
                                     '0206271220',
                                     '1307870147') & dataset_type=='CAMHS')


df_camhs_tooOld=as.data.frame(table(camhs_weird_dob_boards$hb_name))



df_camhs_tooOld %>% 
  #arrange(Freq) %>% 
  ggplot( aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab('Number of CHIs with dob before 1/1/2000 in CAMHS')+
  xlab('')

ggsave("../../../output/tooOld_CAMHS_plots.png",
       width = 16,
       height = 20,
       units = c("cm"),
       dpi = 300,
       bg='white')