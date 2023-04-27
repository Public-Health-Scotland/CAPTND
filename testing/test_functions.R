
library(dplyr)

x <- read_csv("../../../data/testDataset.csv") %>% 
  mutate(CHI = as.character(CHI)) %>% 
  check_chi_captnd(.)

y = null_to_na(x)

y = no_whitespace(x)


hb_name_o = "HB"

y <- correct_hb_names(x)

chi_o = "CHI"

y <- check_chi_captnd(x)

library(readr)
