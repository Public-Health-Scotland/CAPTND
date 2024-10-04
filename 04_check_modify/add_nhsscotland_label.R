
###############################.
### Add 'NHSScotland' label ###
###############################.

# Author: Charlie Smith
# Date: 2024-10-02

# Vectorised solution probably simplest


# df_test <- data.frame(
#   hb_name = c("NHS Scotland", "NHS Fife"),
#   hb = c("NHS Scotland", "NHS Fife"),
#   HB = c("NHS Scotland", "NHS Fife"),
#   `health board` = c("NHS Scotland", "NHS Fife")
# )

add_nhsscotland_label <- function(df){
  
  df[df == "NHS Scotland"] <- "NHSScotland"
  
  return(df)
  
}

# df_updated <- add_nhsscotland_label(df = df_test)
