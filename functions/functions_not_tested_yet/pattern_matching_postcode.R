library(stringr)

x=c('EH2b 8FFa', 'AB12  - 8WH.', 'A14 6RR!', 'G1 6HHq')

str_replace_all(x, '[\\s\\b.+:punct:-]+|(?<=[:alpha:]{2}).$*', '')

