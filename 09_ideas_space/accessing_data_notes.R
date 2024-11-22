
######################################.
### Notes on Accessing CAPTND Data ###
######################################.

# Author: Charlie Smith
# Date: 2024-11-21

# Purpose: useful reference for Paul Bett in accessing CAPTND for DQ reports






# 1 - Access raw SWIFT data directly from DB ------------------------------
# NB this method is slow and unwieldy...

# 1.1 - Establish database connection -------------------------------------
con <- dbConnect(odbc(),
                 dsn = "CAPTND",
                 uid = askForPassword("Enter network username:"), 
                 pwd = askForPassword("Enter network password:"))


# 1.2 - Load data from staging area and join CAMHS and PT sheets ----------

# SWIFT CAMHS
swift_camhs <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_CAMHS"))) 

# SWIFT PT
swift_pt <- as.data.frame(tbl(con, in_schema("CAPTND", "CAPTND_PT")))

# 1.3 -  Join CAMHS and PT ------------------------------------------------
df_swift_raw <- rbind.fill(swift_camhs, swift_pt) 
rm(swift_camhs, swift_pt)


# 2 - Access saved swift data with tidied names ---------------------------
# NB this is fast but the data is unoptimised (the column names have been standardised though)
df_swift_tidy_names <- read_parquet(paste0(root_dir,"/swift_extract.parquet"))


# 3 - Load all CAPTND (SWIFT and globalscape) optimised -------------------
# NB this is fast and the data is optimised (the column names have been standardised too)
df_opti <- read_parquet(paste0(root_dir,'/swift_glob_completed_rtt.parquet')) 




