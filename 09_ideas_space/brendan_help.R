
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
  mutate(rank_comp = lag(rank, n = 1),
         is_same = rank == rank_comp) #|>

  # look at only months/hbs with change in rank
  #filter(is_same == FALSE)
