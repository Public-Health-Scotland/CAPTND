
############################################.
### Function to load synthetic test data ###
############################################.


load_test_data <- function() {
  
  df <- read_csv("../../../data/testDataset_lowercase.csv")
  
  return(df)
  
}

