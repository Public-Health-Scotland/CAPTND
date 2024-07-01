
################################################.
### Remove leading zeros from treatment info ###
################################################.

# Author: Charlie Smith
# Date: 2024-07-01


fix_treatment_leadings_zeros <- function(df){
  
  df_test <- df
    
  df_test$TREAT1[df_test$TREAT1 == "096"] <- "96"
  df_test$TREAT1[df_test$TREAT1 == "098"] <- "98"
  df_test$TREAT1[df_test$TREAT1 == "099"] <- "99"
  
  df_test$TREAT2[df_test$TREAT2 == "096"] <- "96"
  df_test$TREAT2[df_test$TREAT2 == "098"] <- "98"
  df_test$TREAT2[df_test$TREAT2 == "099"] <- "99"
  
  df_test$TREAT3[df_test$TREAT3 == "096"] <- "96"
  df_test$TREAT3[df_test$TREAT3 == "098"] <- "98"
  df_test$TREAT3[df_test$TREAT3 == "099"] <- "99"
  
  return(df_test)
  
}
