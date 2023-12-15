#library(dplyr)
source('setup/save_df_as_parquet.R')
source('check_modify/add_rtt_eval.R')

#get last df pre RTT eval
df=read_parquet(paste0(root_dir,'/swift_glob_completed.parquet'))

#do rtt eval
df_eval=add_rtt_eval(df, evalAllData = TRUE)

#save as parquet
save_as_parquet(df_eval,paste0(root_dir,'/swift_glob_completed_rtt_allData'))

