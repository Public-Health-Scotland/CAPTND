dob_from_chi(min_date = ymd"19200101")



dob_vec=x$dob

dob_vec=dob_vec[!is.na(dob_vec)]
dob_vec=dob_vec[order(dob_vec)]

min(dob_vec)


dob_vec_top = tail(dob_vec, 78)
print(dob_vec_top)


#max real date is "2021-03-09"
#min nate is "1920-12-01"