
###########################################.
### Pull and stack CAPTND from database ###
###########################################.

# Author: Charlie Smith
# Date: 2024-04-01


pull_captnd_from_db <- function(){
  
  con <- dbConnect(odbc::odbc(),
                   dsn = "CAPTND",
                   uid = rstudioapi::askForPassword("Enter network username:"), 
                   pwd = rstudioapi::askForPassword("Enter network password:"))
  
  swift_camhs <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_CAMHS"))) |> 
    rename_swift_columns()
  
  swift_pt <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_PT"))) |> 
    rename_swift_columns()
  
  df_captnd_pre <- rbind.fill(swift_camhs, swift_pt) 
  
  return(df_captnd_pre)
  
}



