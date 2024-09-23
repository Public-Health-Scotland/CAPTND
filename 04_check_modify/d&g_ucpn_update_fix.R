
###########################.
### D&G UCPN Update Fix ###
###########################.

# Author: Charlie Smith
# Date: 2024-09-13

# Issue: D&G PT moving to Morse from early October 2024, meaning that PT will adopt a UCPN. 
# This will mean that pre-existing records' UCPNs will no longer match. Phil will
# provide the old-style UCPN in the UPI field. 

# Solution: for submission from 2024-10-24 onwards, ensure old UCPN (in UPI field)
# for D&G pathways IF pwathway has info from prior 2024-10-24.


# Create a test dataset to test function on:
# ~ 

df_test_data <- as.data.frame(
  
  header_ref_date <- c("", "", "", ""),
  
  hb_name <- c("NHS Dumfries and Galloway", "NHS Dumfries and Galloway", "NHS Dumfries and Galloway", "NHS Fife"),
  
  dataset_type <- c("PT", "PT", "PT", "PT"),
  
  ucpn <- c("12345", "12345", "62891", "62891"),
  
  upi <- c("2639123", "12345", "62891", "62891"),
  
  ref_rec_date <- c("", "", "", "")
  
)


