

df_sub_source <- df |> 
  group_by(!!!syms(data_keys)) |> 
  filter(any(header_month >= "2024-07-01")) |> 
  arrange(ucpn, header_month)|> 
  mutate(sub_source_eval = as.factor(sub_source_eval))

summary(df_sub_source$header_month)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2020-02-01" "2024-03-01" "2024-07-01" "2024-05-03" "2024-10-01" "2024-12-01" 

summary(df_sub_source$sub_source_eval)
#           only swift          globalscape and swift 
#           799546              2314 


df_sub_source2 <- df_sub_source |> 
  filter(sub_source_eval == "globalscape and swift")

length(unique(df_sub_source2$ucpn))
# only 30 pathways - grampian, highland, d&g, forth valley and a&a

summary(df_sub_source2$header_month)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2020-02-01" "2021-10-01" "2022-08-01" "2022-08-19" "2023-10-01" "2024-12-01"
#majority are older records

summary(df_sub_source2$ref_rec_date_opti)
#        Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#"2021-08-03" "2022-01-14" "2022-05-18" "2022-06-17" "2022-12-05" "2024-12-06" 
# so even though the records have been filtered for ref dates >2021-08-01 pathways still have activity from before then - presumably records where ref info was only added when swift came into use

#from eyeball data is messy - pathway with no activity since 2022 which has a ref record for december 2024 - ucpn = 101011172027Y
# pathway which seems to still be active with app dates back in 2019 but ref date in 2023 - ucpn = 101018279638X

df_sub_source3 <- df_sub_source2 |> 
  filter(ref_rec_date_opti > first_treat_app) |> 
  slice(1)
# 12 of 30 pathways have ref date after treatment start

# data seems of sufficiently poor quality and minimal numbers that it would probably be ok to exclude globalscape
#... but there is definitely current activity that we would be missing if we excluded all globalscape records, admittedly not very many pathways


# next question to address - how much current activity are we missing by having the date filters in place?
