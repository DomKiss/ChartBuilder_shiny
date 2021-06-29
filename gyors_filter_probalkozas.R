library(bsearchtools)
library(sqldf)
categories_dfi <- DFI(categories_df, "ISIN_KOD")

start.time <- Sys.time()
DFI.subset(categories_dfi, EQ("ISIN_KOD", "HU0000704507"))
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken

timeseries_dfi <- DFI(timeseries_df, "DATUM")

start.time <- Sys.time()
DFI.subset(timeseries_dfi, RG("DATUM", "2014-04-04", "2015-04-04")) %>% DFI.subset(EQ("ISIN_KOD", "HU0000704507"))
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken


start.time <- Sys.time()
sqldf("select * FROM timeseries_df WHERE DATUM>='2014-04-04'
      AND DATUM<= '2020-04-04'")
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
time.taken





