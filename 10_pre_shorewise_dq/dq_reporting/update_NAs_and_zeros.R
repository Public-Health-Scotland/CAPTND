#############################.
### Update NAs and Zeros ###
#############################.

# Author: Alan Coventry
# Email: alan.coventry@phs.scot
# Date: 2025-08-06
## converts 0 to "-", and NA to ". ."

update_nas_and_zeros <- function(df) {
  cols <- c("count", "total", "proportion")
  
  df[cols] <- lapply(df[cols], function(col) {
    sapply(col, function(x) {
      if (is.na(x)) {
        return(". .")
      } else if (x == 0) {
        return("-")
      } else {
        return(as.character(x))
      }
    })
  })
  
  return(df)
}
