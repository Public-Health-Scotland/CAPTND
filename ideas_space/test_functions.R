
# library(dplyr)

x <- read_csv("../../../data/testDataset.csv") %>% 
  mutate(CHI = as.character(CHI)) %>% 
  check_chi_captnd(.)

x1 <- x %>% 
  select(CHI) %>% 
  distinct()

y1 <- y %>% 
  select(CHI) %>% 
  mutate(CHI = as.character(CHI)) %>% 
  distinct()

z <- anti_join(y1, x1)


x <- read_csv("../../../data/testDataset.csv")

y = null_to_na(x)

y = no_whitespace(x)


hb_name_o = "HB"

y <- correct_hb_names(x)

chi_o = "CHI"

y <- check_chi_captnd(x)

library(readr)


x <- read_csv("../../../data/testDataset.csv") %>% 
  mutate(CHI = as.character(CHI))

chi_o = "CHI"
y <- check_dob_from_chi(x)


