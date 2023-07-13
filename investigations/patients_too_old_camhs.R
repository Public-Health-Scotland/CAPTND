camhs_weird_dob=df_swift_clean2 %>% filter(dob<today(),dataset_type=='CAMHS')

camhs_weird_dob_boards = camhs_weird_dob %>% 
  filter(dob<ymd(20000101)) %>% 
  select(hb_name, dataset_type, chi) %>% 
  distinct()
df_camhs_tooOld=as.data.frame(table(camhs_weird_dob_boards$hb_name))


df_camhs_tooOld %>% 
  arrange(Freq) %>% 
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