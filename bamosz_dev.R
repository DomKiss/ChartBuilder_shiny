library(rvest)
library(XML)
library(RSelenium)
library(httr)
library(magrittr)
library(taskscheduleR)
library(tidyverse)
library(lattice)
library(tseries)
library(dplyr)
library(readr)
library(DBI)
library(dbplyr)
library(dplyr)
library(odbc)
library(lubridate)
library(zoo)
library(reshape2)



file_names <- c(
                "1995.csv",
                "1996.csv",
                "1997.csv",
                "1998.csv",
                "1999.csv",
                
                "2000.csv",
                "2001.csv",
                "2002.csv",
                "2003.csv",
                "2004.csv",
                
                "2005.csv",
                "2006.csv",
                "2007.csv",
                "2008.csv",
                "2009.csv",
                
                "2010.csv",
                "2011.csv",
                "2012.csv",
                "2013.csv",
                "2014.csv",
                
                "2015.csv",
                "2016.csv",
                "2017a.csv",
                "2017b.csv",
                "2018a.csv",
                
                "2018b.csv",
                "2019a.csv",
                "2019b.csv",
                "2020.csv")


for (x in 1:length(file_names)) {

  print("starting")
  

raw_df <- read.csv2(file_names[x], header=FALSE, sep=";", dec = ",", 
                    stringsAsFactors=FALSE, encoding='ANSI')

raw_df_u <- raw_df[1:14,]
raw_df_l <- raw_df[-(1:14),]
names(raw_df_l) <- unlist( raw_df_l[1,] )
raw_df_l <- raw_df_l[-1,]


#version
download_date <- as.Date(raw_df[1,2], "%Y.%m.%d")
version_val <- gsub("-", "", as.character(download_date))


#temp variables
rownum_u <- nrow(raw_df_u)
rownum_l <- nrow(raw_df_l)
colnum_u <- ncol(raw_df_u)
colnum_l <- ncol(raw_df_l)

dist <- 14
fin_val <- floor(colnum_l/dist)
dates <- raw_df_l[1]

col_names_u <- unname( unlist ( unclass( raw_df_u[1] )[1] ) )



#structure lower and upper table
for (i in 0:(fin_val-1) ) {
  start_col <- i*dist+2
  end_col <- i*dist+dist+1
  isin_code <- raw_df_u[2,start_col]
  
  #categories (upper)
  temp_df <- data.frame( t(raw_df_u[-1,start_col]), stringsAsFactors=FALSE)
  temp_df <- cbind(version=version_val, temp_df, stringsAsFactors=FALSE)
  names(temp_df) <- col_names_u #oszlop?tnevez?s
  if (i==0) {
    categories_df <- temp_df #els? df
  } else {
    categories_df <- rbind(categories_df, temp_df, stringsAsFactors=FALSE) #hozz?told?s
  }
  
  
  #time series (lower)
  temp_df <- raw_df_l[start_col:end_col] #id?sor
  temp_df <- cbind(isin=isin_code, temp_df, stringsAsFactors=FALSE) #isin hozz?csap?sa
  temp_df <- cbind(dates, temp_df, stringsAsFactors=FALSE) #d?tumok hozz?csap?sa
  temp_df <- cbind(version=version_val, temp_df, stringsAsFactors=FALSE)
  if (i==0) {
    time_series_df <- temp_df #els? df
    col_names_l <- c("Verzi?", "D?tum", "ISIN k?d", colnames(time_series_df)[-(1:2)])
  } else {
    time_series_df <- rbind(time_series_df, temp_df, stringsAsFactors=FALSE) #hozz?told?s
  }
}

#final touches
names(categories_df) <- c("VERZIO",
                          "ISIN_KOD",
                          "ALAP_NEVE",
                          "ALAPKEZELO",
                          "LETETKEZELO",
                          "ALAPTIPUS",
                          "ALAPFAJTA",
                          "BEFPOL_SZERINTI_KATEGORIA",
                          "DEVIZALIS_KITETTSEG",
                          "FOLDRAJZI_KITETTSEG",
                          "EGYEB_KITETTSEG",
                          "DEVIZANEM",
                          "STATUSZ",
                          "INDULAS_DATUMA"
)
categories_df <- mutate(categories_df, INDULAS_DATUMA = as.Date(INDULAS_DATUMA, format= "%Y/%m/%d") )

names(time_series_df) <- c("VERZIO",
                           "DATUM",
                           "ISIN_KOD",
                           "NETTO_ESZKOZERTEK",
                           "ARFOLYAM",
                           "KIFIZETETT_HOZAMOK",
                           "NAPI_BEFJEGY_FORGALOM",
                           "NAPI_BEFJEGY_FORGALOM_SZAZALEK",
                           "REFERENCIAINDEX",
                           "EFF_HOZAM_3_HONAPOS",
                           "EFF_HOZAM_6_HONAPOS",
                           "EFF_HOZAM_1_EVES",
                           "EFF_HOZAM_3_EVES",
                           "EFF_HOZAM_5_EVES",
                           "EFF_HOZAM_10_EVES",
                           "EFF_HOZAM_EV_ELEJETOL",
                           "EFF_HOZAM_INDULASTOL"
)
n <- ncol(time_series_df)
time_series_df[time_series_df == ""] <- NA

time_series_df[] <- lapply(time_series_df, gsub, pattern = ",", replacement= ".")
time_series_df <- mutate(time_series_df, DATUM = as.Date(DATUM, format= "%Y/%m/%d") )
time_series_df[4]  <- time_series_df[4] %>% mutate_if(is.character,parse_number)
time_series_df[5]  <- time_series_df[5] %>% mutate_if(is.character,parse_number)
time_series_df[6]  <- time_series_df[6] %>% mutate_if(is.character,parse_number)
time_series_df[7]  <- time_series_df[7] %>% mutate_if(is.character,parse_number)
time_series_df[8]  <- time_series_df[8] %>% mutate_if(is.character,parse_number)/100
time_series_df[9]  <- time_series_df[9] %>% mutate_if(is.character,parse_number)/100
time_series_df[10] <- time_series_df[10] %>% mutate_if(is.character,parse_number)/100
time_series_df[11] <- time_series_df[11] %>% mutate_if(is.character,parse_number)/100
time_series_df[12] <- time_series_df[12] %>% mutate_if(is.character,parse_number)/100
time_series_df[13] <- time_series_df[13] %>% mutate_if(is.character,parse_number)/100
time_series_df[14] <- time_series_df[14] %>% mutate_if(is.character,parse_number)/100
time_series_df[15] <- time_series_df[15] %>% mutate_if(is.character,parse_number)/100
time_series_df[16] <- time_series_df[16] %>% mutate_if(is.character,parse_number)/100
time_series_df[17] <- time_series_df[17] %>% mutate_if(is.character,parse_number)/100


if (x==1) {
  final_time_series_df <- time_series_df
  final_categories_df <- categories_df 
} else {
  final_time_series_df <- rbind(final_time_series_df, time_series_df, stringsAsFactors=FALSE)
  final_categories_df <- rbind(final_categories_df, categories_df, stringsAsFactors=FALSE)
}

print(file_names[x])
print("done")

}

final_categories_df <- categories_df


#final_time_series_df <- read.csv2("final_time_series_df.csv", header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE, encoding='ANSI')
#final_time_series_df <- select(final_time_series_df, 1:17)
#final_time_series_df <- mutate(final_time_series_df, DATUM = as.Date(DATUM))
#final_time_series_df <- mutate(final_time_series_df, VERZIO = "20200828" )
#final_time_series_df <- distinct(final_time_series_df, DATUM, ISIN_KOD, .keep_all=TRUE )
#final_categories_df <- read.csv2("final_categories_df.csv", header=TRUE, sep=",", dec = ".", stringsAsFactors=FALSE, encoding='ANSI')


#n?vekv? sorrend date alapj?n - timeseries
final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM)


#filter out na and arrange
final_time_series_df <- mutate(final_time_series_df, DROP_FLAG = 
                                 is.na(NETTO_ESZKOZERTEK) &
                                 is.na(ARFOLYAM) &
                                 is.na(KIFIZETETT_HOZAMOK) &
                                 is.na(NAPI_BEFJEGY_FORGALOM) &
                                 is.na(REFERENCIAINDEX) &
                                 is.na(EFF_HOZAM_3_HONAPOS)
)
final_time_series_df <- filter(final_time_series_df, DROP_FLAG == FALSE)
final_time_series_df <- select(final_time_series_df, 1:(length(final_time_series_df)-1))
final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM)




#########################

#write.csv(final_categories_df, file = "final_categories_df.csv", row.names=FALSE)
#write.csv(final_time_series_df, file = "final_time_series_df.csv", row.names=FALSE)
#write.csv(final_dates_df, file = "final_dates_df.csv", row.names=FALSE)

#mentsd ki tempbe visszat?lt?shez
backup_timeseries_df <- final_time_series_df
#backup_categories_df <- final_categories_df
#final_time_series_df <- backup_timeseries_df


#final_time_series_df <- dbReadTable(con, "timeseries_final")
#final_time_series_df <- backup_timeseries_df



#d?tum ?s forgalmaz?si napok flag
forg_dates_df <- select(final_time_series_df, 1, 4) #datum ?s NE?
forg_dates_df[is.na(forg_dates_df)] <- 0
forg_dates_df <- aggregate(forg_dates_df["NETTO_ESZKOZERTEK"], by=forg_dates_df["DATUM"], sum)
forg_dates_df <- mutate(forg_dates_df, DATUM = as.Date(DATUM))
forg_dates_df <- arrange(forg_dates_df, DATUM)
forg_dates_df <- mutate(forg_dates_df, FORG_NAP_FLAG = ifelse( abs(NETTO_ESZKOZERTEK/lag(NETTO_ESZKOZERTEK)) > 0.2 , 1 , NA)) #igazabol csak pozitiv lehet - abs feles
forg_dates_df <- select(forg_dates_df, 1, 3) #datum ?s forg_nap_flag
forg_dates_df <- subset(forg_dates_df, FORG_NAP_FLAG == 1)

#d?tumt?bla
dates_vector <- seq(as.Date('1995-01-01'), as.Date('2020-08-28'), by = 'days')
final_dates_df <- data.frame(dates_vector)
names(final_dates_df) <- c("DATUM")
#final_dates_df <- filter(final_dates_df, DATUM < as.Date('2012-01-01'))

#forg nap flag oszlop
final_dates_df <- merge(final_dates_df, forg_dates_df, by="DATUM", all.x=TRUE, sort=TRUE)

#forg datum oszlop
final_dates_df <- mutate(final_dates_df, FORG_DATUM = as.Date( ifelse(FORG_NAP_FLAG==1, DATUM, NA), origin="1970-01-01" ) )
final_dates_df$FORG_DATUM <- na.locf( final_dates_df$FORG_DATUM, na.rm = FALSE )

#kimenteni seg?dtablanak a p?rokat
date_pairs_df <- select(final_dates_df, 1, 3)
names(date_pairs_df) <- c('DATUM_SEGED', 'FORG_DATUM_SEGED' )



#g?rd?l? d?tumok
final_dates_df <- mutate(final_dates_df, TMP = DATUM-7)
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_HET" 
  
final_dates_df <- mutate(final_dates_df, TMP = add_with_rollback(DATUM, months(-1)))
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_HONAP" 
  
final_dates_df <- mutate(final_dates_df, TMP = add_with_rollback(DATUM, months(-3)))
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_NEGYEDEV"
  
final_dates_df <- mutate(final_dates_df, TMP = add_with_rollback(DATUM, years(-1)))
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_1EV"
  
final_dates_df <- mutate(final_dates_df, TMP = add_with_rollback(DATUM, years(-3)))
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_3EV"
  
final_dates_df <- mutate(final_dates_df, TMP = add_with_rollback(DATUM, years(-5)))
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_5EV"
  
final_dates_df <- mutate(final_dates_df, TMP = add_with_rollback(DATUM, years(-10)))
final_dates_df <- merge(final_dates_df, date_pairs_df, 
                        by.x="TMP", by.y="DATUM_SEGED", 
                        all.x=TRUE, sort=FALSE)
  final_dates_df <- select(final_dates_df, -1)
  names(final_dates_df)[length(names(final_dates_df))]<-"LAG_10EV" 
  
final_dates_df <- arrange(final_dates_df, DATUM)

#melyik nap
final_dates_df <- mutate(final_dates_df, NAP_NEVE = wday(DATUM, label=TRUE) )



#utols? napok

#h?t
fridays <- seq(as.Date("1995-01-06"), as.Date('2020-08-28'), by = 'weeks')
fridays <- as.data.frame(fridays)
names(fridays) <- c("DATUM")

temp_df <- select(final_dates_df, 1, 3)
temp_df <- mutate(temp_df, DATUM=as.Date(DATUM))
temp_df <- merge(fridays, temp_df, by="DATUM", all.x=TRUE, sort=TRUE)
temp_df <- mutate(temp_df, UNAP_HET=1)  
temp_df <- select(temp_df, 2:3)
temp_df <- unique(temp_df) #basszameg, ez volt a hi?nyz? sor ami miatt t?bbsz?r?z?dve voltak a napok
temp_df <- mutate(temp_df, FORG_DATUM=as.Date(FORG_DATUM))

final_dates_df <- merge(final_dates_df, temp_df, by.x="DATUM", by.y="FORG_DATUM", all.x=TRUE, sort=TRUE)

#h?nap - negyed?v - ?v
temp_df <- select(final_dates_df, 3)
temp_df <- mutate(temp_df, HONAP = month(FORG_DATUM) )
temp_df <- mutate(temp_df, UNAP_HONAP = ifelse( HONAP != lead(HONAP), 1, NA ) )
temp_df <- filter(temp_df, UNAP_HONAP == 1)
temp_df <- mutate(temp_df, UNAP_NEGYEDEV = ifelse( HONAP %% 3 == 0, 1, NA ) )
temp_df <- mutate(temp_df, UNAP_EV = ifelse( HONAP %% 12 == 0, 1, NA ) )
temp_df <- select(temp_df, 1, 3:5)
temp_df <- mutate(temp_df, FORG_DATUM=as.Date(FORG_DATUM))

final_dates_df <- merge(final_dates_df, temp_df, by.x="DATUM", by.y="FORG_DATUM", all.x=TRUE, sort=TRUE)

#sort a biztons?g kedv??rt
final_dates_df <- arrange(final_dates_df, DATUM)



#hozamsz?m?t?s
final_time_series_df <- merge(final_time_series_df, final_dates_df[,1:2], 
                        by="DATUM", all.x=TRUE, sort=FALSE)
final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM) #esetleg itt ?rdemes megn?zni mik az NA-k!!!!!

temp_hozam_df <- filter(final_time_series_df, FORG_NAP_FLAG == 1)
temp_hozam_df <- filter(temp_hozam_df, !is.na(ARFOLYAM))
temp_hozam_df <- filter(temp_hozam_df, ARFOLYAM > 0.0000001 )
temp_hozam_df <- select(temp_hozam_df, 1:5)
temp_hozam_df <- arrange(temp_hozam_df, VERZIO, ISIN_KOD, DATUM)
temp_hozam_df <- mutate(temp_hozam_df, LOG_HOZAM = ifelse(ISIN_KOD != lag(ISIN_KOD), 
                                                          NA, 
                                                          log( ARFOLYAM/lag(ARFOLYAM) ) ) ) #loghozam
temp_hozam_df <- mutate(temp_hozam_df, EFF_HOZAM = ifelse(ISIN_KOD != lag(ISIN_KOD), 
                                                          NA, 
                                                          ARFOLYAM/lag(ARFOLYAM)-1 ) ) #effektiv hozam
temp_hozam_df <- subset(temp_hozam_df, select= -c(NETTO_ESZKOZERTEK, ARFOLYAM) ) #csak a mergelos oszlopok + hozamok maradjanak
temp_hozam_df <- arrange(temp_hozam_df, VERZIO, ISIN_KOD, DATUM)
temp_hozam_df <- filter(temp_hozam_df, EFF_HOZAM != 0 | LOG_HOZAM != 0 )

final_time_series_df <- merge(final_time_series_df, temp_hozam_df, 
                              by=c("VERZIO","ISIN_KOD","DATUM"), all.x=TRUE, sort=FALSE)
final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM)
final_time_series_df[c("LOG_HOZAM", "EFF_HOZAM")][is.na(final_time_series_df[c("LOG_HOZAM", "EFF_HOZAM")])] <- 0


#final_time_series_df <- backup_time_series_df








# deviza
currency_historic <- read.csv2("currency_historic.csv", header=TRUE, sep=",", dec = ".", 
                    stringsAsFactors=FALSE, encoding='ANSI')
currency_historic <- mutate(currency_historic, DATUM = as.Date(DATUM, "%Y.%m.%d"))
currency_historic <- melt(data = currency_historic, id.vars = c("DATUM"), 
                          measure.vars = c("BGN","CZK","EUR","HUF","PLN","RON","TRY","USD"))
names(currency_historic) <- c("DATUM", "DEVIZANEM", "DEVIZA_ARFOLYAM")
final_currency_df <- currency_historic

final_time_series_df <- merge(final_time_series_df, select(final_categories_df, 2, 12), 
                            by="ISIN_KOD", all.x=TRUE, sort=F)
final_time_series_df <- merge(final_time_series_df, currency_historic, 
                              by=c("DATUM", "DEVIZANEM"), all.x=TRUE, sort=F)
final_time_series_df <- mutate(final_time_series_df, NETTO_ESZKOZERTEK_HUF = 
                                 ifelse(DEVIZANEM=="HUF", NETTO_ESZKOZERTEK, NETTO_ESZKOZERTEK*DEVIZA_ARFOLYAM) )
final_time_series_df <- mutate(final_time_series_df, NAPI_BEFJEGY_FORGALOM_HUF = 
                                 ifelse(DEVIZANEM=="HUF", NAPI_BEFJEGY_FORGALOM, NAPI_BEFJEGY_FORGALOM*DEVIZA_ARFOLYAM) )
final_time_series_df <- subset(final_time_series_df, select = -c(DEVIZANEM, DEVIZA_ARFOLYAM))
final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM)


final_categories_df <- categories_finisher(final_categories_df)
#final_categories_df <- read.csv2("final_categories_df.csv", header=T, sep=",", dec = ".", stringsAsFactors=FALSE, encoding='ANSI')



#### SQL-be felt?lt?s

con <- dbConnect(drv = odbc::odbc(),
                 Driver = "SQL Server", 
                 Server = "projectbamosz2.database.windows.net", 
                 Database = "project_bamosz ",
                 uid = "backend.datavis",
                 pwd = "Vizsla123",
                 port = 1503)

dbWriteTable(con, 
             name = "categories_final", 
             value = final_categories_df, 
             row.names = FALSE,
             overwrite = TRUE,
             field.types = c(
                             ALAP_NEVE = "NVARCHAR(255)", 
                             ALAPKEZELO = "NVARCHAR(255)",
                             LETETKEZELO = "NVARCHAR(255)",
                             ALAPTIPUS = "NVARCHAR(255)",
                             ALAPFAJTA = "NVARCHAR(255)",
                             BEFPOL_SZERINTI_KATEGORIA = "NVARCHAR(255)",
                             DEVIZALIS_KITETTSEG = "NVARCHAR(255)",
                             FOLDRAJZI_KITETTSEG = "NVARCHAR(255)",
                             EGYEB_KITETTSEG = "NVARCHAR(255)",
                             STATUSZ = "NVARCHAR(255)",
                             ALAPKEZELO_ROVIDNEV = "NVARCHAR(255)"
                             )
             )
print("categories_final uploaded")

dbWriteTable(con, 
             name = "dates_final", 
             value = final_dates_df, 
             row.names = FALSE,
             overwrite = TRUE)
print("dates_final uploaded")

dbWriteTable(con, 
             name = "currency_final", 
             value = final_currency_df, 
             row.names = FALSE,
             overwrite = TRUE)
print("currency_final uploaded")

dbWriteTable(con, 
             name = "timeseries_final", 
             value = final_time_series_df, 
             row.names = FALSE,
             overwrite = TRUE)
print("timeseries_final uploaded")





