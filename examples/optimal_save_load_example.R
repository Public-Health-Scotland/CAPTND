
##################################.
### Optimal Saving and Loading ###
##################################.


# 1 - Housekeeping --------------------------------------------------------

# Install required packages
install.packages("tidyverse")
install.packages("arrow")
install.packages("parallelly")

# Load required packages
library(tidyverse)
library(arrow)


# 2 - Get Background Info -------------------------------------------------

# Correctly identify the number of CPUs available to the session
n_cpus <- as.numeric(parallelly::availableCores())

# Tell {arrow} how many CPUs it can use
arrow::set_cpu_count(n_cpus)



# 3 - Save Example --------------------------------------------------------

# Write the 'iris' dataset to a ZStandard compressed parquet file with
# {arrow}
arrow::write_parquet(iris,
                     sink = "./examples/iris.parquet",
                     compression = "zstd")

# save Iris as RDS for comparison
saveRDS(iris,
        file = "./examples/iris.parquet.RDS")

# save Iris as .csv for comparison
library(rio)
export(iris,
       file = "./examples/iris.parquet.csv", format = "csv")


# 4 - Load Saved Data Example -------------------------------------------

# Calculate the mean petal length by species from the parquet file
# written above
data <- arrow::read_parquet(file = "./examples/iris.parqagreuet",
                            col_select = c("Species", "Petal.Length")) |>
  dplyr::group_by(Species) |>
  dplyr::summarise(mean_petal_length = mean(Petal.Length)) |>
  dplyr::collect()




