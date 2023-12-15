
source('config/new_colnames.R')
#library(stringr)

postcode_areas=read_csv('../../../data/postcode_areas.csv')

attend = df_glob_swift_completed %>% 
  filter(!!sym(att_status_o)==8 & 
           !is.na(!!sym(postcode_o))) %>% 
  select(all_of(data_keys),!!postcode_o) %>% 
  distinct() %>% 
  mutate(postcode_start=str_sub(postcode,1,2)) %>% 
  inner_join(postcode_areas, by='postcode_start') %>% 
  ungroup()


gen_attend = attend %>% 
  select(hb_name,postcode_area) %>% 
  distinct()

write_csv(gen_attend, '../../../problems/board_treat_vs_residence_DNA_2023_08_25.csv')



pop_quiz_df=read_csv('../../../problems/board_treat_not_residence_DNA_2023_08_25.csv')

attend_not_hb_area=attend %>% inner_join(pop_quiz_df, by=c('hb_name','postcode_area'))



