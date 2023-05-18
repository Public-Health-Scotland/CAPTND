
##################################.
### Optimal Saving and Loading ###
##################################.


# 1 - Housekeeping --------------------------------------------------------

# Install required packages
install.packages("tidyverse")
install.packages("arrow")
install.packages("parallelly")
install.packages("microbenchmark")

# Load required packages
library(tidyverse)
library(arrow)
library(parallelly)
library(microbenchmark)
library(rio)


# 2 - Get Background Info -------------------------------------------------

# Correctly identify the number of CPUs available to the session
n_cpus <- as.numeric(parallelly::availableCores())

# Tell {arrow} how many CPUs it can use
arrow::set_cpu_count(n_cpus)



# 3 - Save Example --------------------------------------------------------

# Write the 'iris' dataset to a ZStandard compressed parquet file with
# {arrow}


microbenchmark(

arrow::write_parquet(iris,
                     sink = "./examples/iris.parquet",
                     compression = "zstd"),

# save Iris as RDS for comparison
saveRDS(iris,
        file = "./examples/iris.RDS"),

# save Iris as .csv for comparison

export(iris,
       file = "./examples/iris.csv", format = "csv")

)


# 4 - Load Saved Data Example -------------------------------------------

# Calculate the mean petal length by species from the parquet file
# written above
microbenchmark(

data <- arrow::read_parquet(file = "./examples/iris.parquet",
                            col_select = c("Species", "Petal.Length")) |>
  dplyr::group_by(Species) |>
  dplyr::summarise(mean_petal_length = mean(Petal.Length)) |>
  dplyr::collect()

)



# 5 - Testing at scale ----------------------------------------------------

df_2 <- iris %>% 
  head(100)

df_3 <- df_2 %>% 
  slice(rep(row_number(), 10))

df_4 <- df_2 %>% 
  slice(rep(row_number(), 100))

df_5 <- df_2 %>% 
  slice(rep(row_number(), 1000))

df_6 <- df_2 %>% 
  slice(rep(row_number(), 10000))


microbenchmark(
  
  write_parquet(df_2,
                sink = "./examples/df_10_2.parquet",
                compression = "zstd"),
  
  write_parquet(df_3,
                sink = "./examples/df_10_3.parquet",
                compression = "zstd"),
  
  write_parquet(df_4,
                sink = "./examples/df_10_4.parquet",
                compression = "zstd"),
  
  write_parquet(df_5,
                sink = "./examples/df_10_5.parquet",
                compression = "zstd"),
  
  write_parquet(df_6,
                sink = "./examples/df_10_6.parquet",
                compression = "zstd")
  
)


microbenchmark(
  
  saveRDS(df_6, "./examples/df_10_6.RDS"),
  
  write_parquet(df_6,
                sink = "./examples/df_10_6.parquet",
                compression = "zstd")
  
)





