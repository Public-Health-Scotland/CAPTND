
x=df_glob_raw$glob_apps

x1=x %>% filter(location==1)

x1_withCHI= x1 %>% filter(!is.na(chi))
#3986 records

x1_noCHI= x1 %>% filter(is.na(chi))
#3 records