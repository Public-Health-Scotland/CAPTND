# CAMHS unadjusted, in order
as.Date("27/04/2022", "%d/%m/%Y") - as.Date("15/02/2022", "%d/%m/%Y") 
# 71

as.Date("21/08/2023", "%d/%m/%Y") - as.Date("07/12/2022", "%d/%m/%Y") 
# 257

as.Date("07/07/2023", "%d/%m/%Y") - as.Date("08/11/2022", "%d/%m/%Y") 
# 241

as.Date("15/08/2023", "%d/%m/%Y") - as.Date("16/01/2023", "%d/%m/%Y") 
# 211

as.Date("18/10/2023", "%d/%m/%Y") - as.Date("27/04/2023", "%d/%m/%Y") 
# 174

as.Date("15/09/2022", "%d/%m/%Y") - as.Date("15/07/2022", "%d/%m/%Y") 
# 62

as.Date("03/04/2024", "%d/%m/%Y") - as.Date("07/12/2023", "%d/%m/%Y") 
# 118

as.Date("08/06/2023", "%d/%m/%Y") - as.Date("08/01/2023", "%d/%m/%Y") 
# 151

as.Date("23/10/2023", "%d/%m/%Y") - as.Date("27/01/2023", "%d/%m/%Y") 
# 269

as.Date("01/10/2023", "%d/%m/%Y") - as.Date("15/06/2023", "%d/%m/%Y") 
# 108

as.Date("26/04/2024", "%d/%m/%Y") - as.Date("26/11/2023", "%d/%m/%Y") 
# 152

as.Date("04/01/2023", "%d/%m/%Y") - as.Date("16/08/2022", "%d/%m/%Y") 
# 141

as.Date("03/02/2023", "%d/%m/%Y") - as.Date("16/07/2022", "%d/%m/%Y") 
# 202

as.Date("09/04/2024", "%d/%m/%Y") - as.Date("16/07/2023", "%d/%m/%Y") 
# 268

as.Date("30/06/2023", "%d/%m/%Y") - as.Date("19/04/2023", "%d/%m/%Y") 
# 72

as.Date("30/10/2022", "%d/%m/%Y") - as.Date("24/08/2022", "%d/%m/%Y") 
# 67

as.Date("14/07/2022", "%d/%m/%Y") - as.Date("29/01/2022", "%d/%m/%Y") 
# 166

as.Date("31/03/2024", "%d/%m/%Y") - as.Date("23/11/2023", "%d/%m/%Y") 
# 129



# PT unadjusted, in order

as.Date("15/06/2022", "%d/%m/%Y") - as.Date("28/01/2022", "%d/%m/%Y") 
# 138

as.Date("17/06/2022", "%d/%m/%Y") - as.Date("02/11/2021", "%d/%m/%Y") 
# 227

as.Date("25/01/2024", "%d/%m/%Y") - as.Date("22/10/2021", "%d/%m/%Y") 
# 825

as.Date("18/02/2022", "%d/%m/%Y") - as.Date("20/08/2021", "%d/%m/%Y") 
# 182

as.Date("26/04/2023", "%d/%m/%Y") - as.Date("28/01/2023", "%d/%m/%Y") 
# 88

as.Date("11/11/2023", "%d/%m/%Y") - as.Date("28/06/2023", "%d/%m/%Y") 
# 136

as.Date("09/09/2023", "%d/%m/%Y") - as.Date("21/03/2023", "%d/%m/%Y") 
# 172

as.Date("26/04/2024", "%d/%m/%Y") - as.Date("26/11/2023", "%d/%m/%Y") 
# 152

as.Date("03/04/2024", "%d/%m/%Y") - as.Date("28/09/2023", "%d/%m/%Y") 
# 188

as.Date("25/09/2023", "%d/%m/%Y") - as.Date("01/06/2023", "%d/%m/%Y") 
# 116

as.Date("26/03/2023", "%d/%m/%Y") - as.Date("04/03/2023", "%d/%m/%Y") 
# 22

as.Date("04/08/2023", "%d/%m/%Y") - as.Date("01/02/2023", "%d/%m/%Y") 
# 184

as.Date("21/04/2024", "%d/%m/%Y") - as.Date("04/09/2023", "%d/%m/%Y") 
# 230

as.Date("03/03/2023", "%d/%m/%Y") - as.Date("15/07/2022", "%d/%m/%Y") 
# 231

as.Date("02/11/2023", "%d/%m/%Y") - as.Date("08/05/2023", "%d/%m/%Y") 
# 178

as.Date("12/01/2024", "%d/%m/%Y") - as.Date("04/01/2023", "%d/%m/%Y") 
# 373





# adjusted manual

# CAMHS adjusted

#107001093126P
as.Date("27/04/2022", "%d/%m/%Y") - as.Date("15/02/2022", "%d/%m/%Y") 
# 71
# has 33 unav days, no dates

#101028182502J
as.Date("21/08/2023", "%d/%m/%Y") - as.Date("07/12/2022", "%d/%m/%Y")
# 257
as.Date("07/12/2022", "%d/%m/%Y") + 126
#"2023-04-12" guarantee - unavailability outside guarantee

#107001244641U
as.Date("07/07/2023", "%d/%m/%Y") - as.Date("08/11/2022", "%d/%m/%Y") 
# 241
as.Date("08/11/2022", "%d/%m/%Y") + 126
# "2023-03-14" guarantee - dnas and unav outside guarantee

#101028457061N
as.Date("15/08/2023", "%d/%m/%Y") - as.Date("16/01/2023", "%d/%m/%Y") 
# 211
as.Date("16/01/2023", "%d/%m/%Y") + 126
# "2023-05-22" guarantee
# clock reset on 2023-05-19 and again 2023-05-29
as.Date("15/08/2023", "%d/%m/%Y") - as.Date("29/05/2023", "%d/%m/%Y")
# 78 from clock to first treatment
as.Date("29/05/2023", "%d/%m/%Y") + 126
# "2023-10-02" new guarantee
# and unavailability 26/05 to 12/07 straddling clock reset date so use 29/05
as.Date("12/07/2023", "%d/%m/%Y") - as.Date("29/05/2023", "%d/%m/%Y")
# 44 days unav
78 - 44
# 34 days

#1.07001E+12
as.Date("18/10/2023", "%d/%m/%Y") - as.Date("27/04/2023", "%d/%m/%Y") 
# 174
as.Date("27/04/2023", "%d/%m/%Y") + 126
# "2023-08-31" guarantee
# unavailability within guarantee 19/07 to 31/07
as.Date("31/07/2023", "%d/%m/%Y") - as.Date("19/07/2023", "%d/%m/%Y") 
# 12 days unav
174 - 12
# 162 days

#1111111CAMHS
as.Date("15/09/2022", "%d/%m/%Y") - as.Date("15/07/2022", "%d/%m/%Y") 
# 62
# basic rtt

#2222222CAMHS
as.Date("03/04/2024", "%d/%m/%Y") - as.Date("07/12/2023", "%d/%m/%Y") 
# 118
# clinician cancelled 03/03 no adj

#3333333CAMHS
as.Date("08/06/2023", "%d/%m/%Y") - as.Date("08/01/2023", "%d/%m/%Y") 
# 151
as.Date("08/01/2023", "%d/%m/%Y") + 126
# "2023-05-14" guarantee
# DNA 08/03 within guarantee so reset
as.Date("08/06/2023", "%d/%m/%Y") - as.Date("08/03/2023", "%d/%m/%Y")
# 92 days

#4444444CAMHS
as.Date("23/10/2023", "%d/%m/%Y") - as.Date("27/01/2023", "%d/%m/%Y") 
# 269
as.Date("27/01/2023", "%d/%m/%Y") + 126
# "2023-06-02" guarantee
# unav 13/03 - 13/04 within guarantee, unav 21/09-21/10 outside
as.Date("13/04/2023", "%d/%m/%Y") - as.Date("13/03/2023", "%d/%m/%Y") 
# 31 days unav
269 - 31
# 238 days

#5555555CAMHS
as.Date("01/10/2023", "%d/%m/%Y") - as.Date("15/06/2023", "%d/%m/%Y") 
# 108
as.Date("15/06/2023", "%d/%m/%Y") + 126
# "2023-10-19" guarantee 
# dna 14/08 so reset
as.Date("01/10/2023", "%d/%m/%Y") - as.Date("14/08/2023", "%d/%m/%Y") 
# 48
as.Date("14/08/2023", "%d/%m/%Y") + 126
# "2023-12-18" new guarantee
# unav 21/08 - 30/09
as.Date("30/09/2023", "%d/%m/%Y") - as.Date("21/08/2023", "%d/%m/%Y")
# 40 days unav
48 - 40
# 8 days

# 6666666CAMHS
as.Date("26/04/2024", "%d/%m/%Y") - as.Date("26/11/2023", "%d/%m/%Y") 
# 152
as.Date("26/11/2023", "%d/%m/%Y") + 126
# "2024-03-31" guarantee
# unav outside 18 weeks, starting after 01/04 new rule date but CAMHS

#7777777CAMHS
as.Date("04/01/2023", "%d/%m/%Y") - as.Date("16/08/2022", "%d/%m/%Y") 
# 141
as.Date("16/08/2022", "%d/%m/%Y") + 126
# "2022-12-20" guarantee date
# cna 21/10 so reset
as.Date("04/01/2023", "%d/%m/%Y") - as.Date("21/10/2022", "%d/%m/%Y") 
# 75 days

#8888888CAMHS
as.Date("03/02/2023", "%d/%m/%Y") - as.Date("16/07/2022", "%d/%m/%Y") 
# 202
as.Date("16/07/2022", "%d/%m/%Y") + 126
# "2022-11-19" guarantee
# cna 21/11 outside guarantee

#9999999CAMHS
as.Date("09/04/2024", "%d/%m/%Y") - as.Date("16/07/2023", "%d/%m/%Y") 
# 268
as.Date("16/07/2023", "%d/%m/%Y") + 126
# "2023-11-19" guarantee
# cnw 21/09/23 so reset
as.Date("09/04/2024", "%d/%m/%Y") - as.Date("21/09/2023", "%d/%m/%Y") 
# 201
as.Date("21/09/2023", "%d/%m/%Y") + 126
# "2024-01-25" new guarantee
# second cnw 01/02 outside guarantee

#2221111CAMHS
as.Date("30/06/2023", "%d/%m/%Y") - as.Date("19/04/2023", "%d/%m/%Y") 
# 72
# dna after treatment start

#3331111CAMHS
as.Date("30/10/2022", "%d/%m/%Y") - as.Date("24/08/2022", "%d/%m/%Y") 
# 67
as.Date("24/08/2022", "%d/%m/%Y") + 126
# "2022-12-28" guarantee
# cnq 11/10 so reset
as.Date("30/10/2022", "%d/%m/%Y") - as.Date("11/10/2022", "%d/%m/%Y")
# 19 days
# unav after treatment start

# 4441111CAMHS
as.Date("14/07/2022", "%d/%m/%Y") - as.Date("29/01/2022", "%d/%m/%Y") 
# 166
as.Date("29/01/2022", "%d/%m/%Y") + 126
# "2022-06-04" guarantee
# dna 01/05 so reset
as.Date("14/07/2022", "%d/%m/%Y") - as.Date("01/05/2022", "%d/%m/%Y")
# 74 days
# second dna after treatment start

# 5551111CAMHS
as.Date("31/03/2024", "%d/%m/%Y") - as.Date("23/11/2023", "%d/%m/%Y") 
# 129
as.Date("23/11/2023", "%d/%m/%Y") + 126
# "2024-03-28" guarantee
# cna 21/02 so reset
as.Date("31/03/2024", "%d/%m/%Y") - as.Date("21/02/2024", "%d/%m/%Y") 
# 39 days
# open ended unav and no unav days so no adj FOR NOW - could use subsequent app date
# dna after treatment start




# PT adjusted

#107001084274N
as.Date("15/06/2022", "%d/%m/%Y") - as.Date("28/01/2022", "%d/%m/%Y") 
# 138
as.Date("28/01/2022", "%d/%m/%Y") + 126
# "2022-06-03" guarantee date
# unav 25/04 - 09/05
as.Date("09/05/2022", "%d/%m/%Y") - as.Date("25/04/2022", "%d/%m/%Y") 
# 14 days unav
138 - 14
# 124 days

#82163088
as.Date("17/06/2022", "%d/%m/%Y") - as.Date("02/11/2021", "%d/%m/%Y") 
# 227
as.Date("02/11/2021", "%d/%m/%Y") + 126
# "2022-03-08" guarantee
# dna 18/02 so reset
as.Date("17/06/2022", "%d/%m/%Y") - as.Date("18/02/2022", "%d/%m/%Y") 
# 119 days
# unav before reset disregarded

#82603890
as.Date("25/01/2024", "%d/%m/%Y") - as.Date("22/10/2021", "%d/%m/%Y") 
# 825
as.Date("22/10/2021", "%d/%m/%Y") + 126
# "2022-02-25" guarantee
# cnas 20/02 so reset, then 27/04, then weekly for 2 months so last reset date is 22/06
as.Date("25/01/2024", "%d/%m/%Y") - as.Date("22/06/2022", "%d/%m/%Y") 
# 582 days
# open-ended unav before reset - IGNORE

#1.07001E+12
as.Date("18/02/2022", "%d/%m/%Y") - as.Date("20/08/2021", "%d/%m/%Y") 
# 182
as.Date("20/08/2021", "%d/%m/%Y")  + 126
# "2021-12-24" guarantee
# unav 20/12 - 17/01 - straddles guarantee date so valid
as.Date("17/01/2022", "%d/%m/%Y") - as.Date("20/12/2021", "%d/%m/%Y") 
# 28 days unav
182 - 28
# 154 days

#1111111111PT
as.Date("26/04/2023", "%d/%m/%Y") - as.Date("28/01/2023", "%d/%m/%Y") 
# 88
# basic rtt no adj

#2222222222PT
as.Date("11/11/2023", "%d/%m/%Y") - as.Date("28/06/2023", "%d/%m/%Y") 
# 136
# clinician cancels 24/09 so ignore

#3333333333PT
as.Date("09/09/2023", "%d/%m/%Y") - as.Date("21/03/2023", "%d/%m/%Y") 
# 172
as.Date("21/03/2023", "%d/%m/%Y") + 126
# "2023-07-25" guarantee
# unav from 30/07 so outside guarantee

#4444444444PT
as.Date("26/04/2024", "%d/%m/%Y") - as.Date("28/11/2023", "%d/%m/%Y") 
# 150
as.Date("28/11/2023", "%d/%m/%Y") + 126
# "2024-04-02" guarantee
# unav 03/04 - 23/04 outside 18 weeks but after 01/04 new rule date for PT
as.Date("23/04/2024", "%d/%m/%Y") - as.Date("03/04/2024", "%d/%m/%Y")
# 20 days unav
150 - 20
# 130 days

#5555555555PT
as.Date("03/04/2024", "%d/%m/%Y") - as.Date("28/09/2023", "%d/%m/%Y") 
# 188
as.Date("28/09/2023", "%d/%m/%Y") + 126
# "2024-02-01" guarantee date
# dna 03/03 outside guarantee date

#6666666666PT
as.Date("25/09/2023", "%d/%m/%Y") - as.Date("01/06/2023", "%d/%m/%Y") 
# 116
as.Date("01/06/2023", "%d/%m/%Y") + 126
# "2023-10-05" guarantee
# cna 03/08 so reset
as.Date("25/09/2023", "%d/%m/%Y") - as.Date("03/08/2023", "%d/%m/%Y") 
# 53 days

#7777777777PT
as.Date("26/03/2023", "%d/%m/%Y") - as.Date("04/03/2023", "%d/%m/%Y") 
# 22
# cCBT basic RTT

#8888888888PT
as.Date("04/08/2023", "%d/%m/%Y") - as.Date("01/02/2023", "%d/%m/%Y") 
# 184
as.Date("01/02/2023", "%d/%m/%Y") + 126
# "2023-06-07" guarantee
# CNW 04/04 so reset
as.Date("04/08/2023", "%d/%m/%Y") - as.Date("04/04/2023", "%d/%m/%Y") 
# 122 days

#9999999999PT
as.Date("21/04/2024", "%d/%m/%Y") - as.Date("04/09/2023", "%d/%m/%Y") 
# 230
as.Date("04/09/2023", "%d/%m/%Y") + 126
# "2024-01-08" guarantee
# CNW 28/01 so outside guarantee

#222111111PT
as.Date("03/03/2023", "%d/%m/%Y") - as.Date("15/07/2022", "%d/%m/%Y") 
# 231
as.Date("15/07/2022", "%d/%m/%Y") + 126
# "2022-11-18" guarantee
# CNA 02/09 so reset
as.Date("03/03/2023", "%d/%m/%Y") - as.Date("02/09/2022", "%d/%m/%Y") 
# 182
as.Date("02/09/2022", "%d/%m/%Y") + 126
# "2023-01-06" new guarantee
# unavailability for 28 days from 03/09
182 - 28
# 154 days
# clinician cancel 14/12 so ignore, dna 21/02 outside new guarantee

#333111111PT
as.Date("02/11/2023", "%d/%m/%Y") - as.Date("08/05/2023", "%d/%m/%Y") 
# 178
as.Date("08/05/2023", "%d/%m/%Y")  + 126
# "2023-09-11" guarantee
# unav from 01/09 - 03/10 spans guarantee so pause
as.Date("03/10/2023", "%d/%m/%Y") - as.Date("01/09/2023", "%d/%m/%Y")
# 32 days unav
178 - 32
# 146 days

#444111111PT
as.Date("12/01/2024", "%d/%m/%Y") - as.Date("04/01/2023", "%d/%m/%Y") 
# 373
as.Date("04/01/2023", "%d/%m/%Y") + 126
# "2023-05-10" guarantee
# dna 16/04 so reset
as.Date("16/04/2023", "%d/%m/%Y") + 126
# "2023-08-20" new guarantee
# dna 31/07 so reset again
as.Date("12/01/2024", "%d/%m/%Y") - as.Date("31/07/2023", "%d/%m/%Y") 
# 165
as.Date("31/07/2023", "%d/%m/%Y") + 126
# "2023-12-04" new guarantee
# unav 22/09 - 21/12 so pause
as.Date("21/12/2023", "%d/%m/%Y") - as.Date("22/09/2023", "%d/%m/%Y") 
# 90 days unav
165 - 90
# 75 days





as.Date("", "%d/%m/%Y") - as.Date("", "%d/%m/%Y") 

