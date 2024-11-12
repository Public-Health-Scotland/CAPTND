
###################################################.
### Spot changes in monthly HB referral numbers ###
###################################################.

# Author: Charlie Smith
# Date: 2024-11-05


# create test data
df_test <- 
  
  # create test data
  data.frame(
    month = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
    hb = c("a", "b", "c", "a", "b", "c",
           "a", "b", "c", "a", "b", "c"),
    value = c(5, 4, 2, 22, 20, 18, 6, 3, 2, 25, 27, 19)) |> 
  
  # add rank per month
  group_by(month) |> 
  arrange(month, -value) |> 
  mutate(rank = row_number()) |>
  
  # get monthly rank per hb
  arrange(hb) |> 
  ungroup() |> 
  
  # create lag column and compare
  group_by(hb) |> 
  mutate(rank_prev = lag(rank, n = 1),
         #rank_check = rank == rank_prev,
         rank_check = case_when(
           rank == rank_prev ~ "same",
           rank != rank_prev ~ "change",
           TRUE ~ NA_character_),
         value_prev = lag(value, n = 1),
         value_diff = value - value_prev,
         value_check = case_when(
           
           value < value_prev ~ "decrease",
           value > value_prev ~ "increase",
           value == value_prev ~ "no change",
           TRUE ~ NA_character_)) #|>

  # look at only months/hbs with change in rank
  #filter(is_same == FALSE)
