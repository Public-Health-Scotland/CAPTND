#######################################################.
### Test for correcting dob format in test dataset  ###
#######################################################.


# 1 Load function ----------------------------------------------------------

source('functions/testing/dob_format_testdata.R')
library(readr)
library(dplyr)
library(lubridate)


# 2 Test --------------------------------------------------------------

df=read_csv('../../../data/testDataset.csv')

df_test=dob_format(df)

#expected: 6 NAs, 72 formatted yyyy-mm-dd


