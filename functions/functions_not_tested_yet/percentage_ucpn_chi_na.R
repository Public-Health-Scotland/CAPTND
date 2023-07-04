df_swift_raw

total_rows_raw=nrow(df_swift_raw)


nrow_na_ucpn = nrow(df_swift_raw %>% filter(is.na(!!sym(ucpn_o))))

nrow_na_chi = nrow(df_swift_raw %>% filter(is.na(!!sym(chi_o))))

nrow_na_chi_and_ucpn = nrow(df_swift_raw %>% filter(is.na(!!sym(ucpn_o))& is.na(!!sym(chi_o))))
nrow_na_chi_or_ucpn = nrow(df_swift_raw %>% filter(is.na(!!sym(ucpn_o))| is.na(!!sym(chi_o))))

df_percentage_usable = data.frame('totalRowsRaw'=c(total_rows_raw,total_rows_raw,total_rows_raw,total_rows_raw),
                                  'rowsRemoved'=c(nrow_na_ucpn,nrow_na_chi,nrow_na_chi_and_ucpn,nrow_na_chi_or_ucpn),
                                  'reasonRemoval'=c('ucpn_na','chi_na','chi_and_ucpn_na','chi_or_ucpn_na')) %>% 
  mutate(percentageRemoved=(100*rowsRemoved)/totalRowsRaw)

