
#install.packages("rvest")
#install.packages("RSelenium", dependencies = TRUE)
#install.packages("XML", repos = "http://www.omegahat.net/R")
#install.packages("taskscheduleR")


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
library(odbc)
library(quantmod)
library(stringi)
library(lubridate)
library(zoo)
library(reshape2)






####################################################################
# creating the connection to the database

con <- dbConnect(drv = odbc::odbc(),
                 Driver = "SQL Server", 
                 Server = "projectbamosz2.database.windows.net", 
                 Database = "project_bamosz ",
                 uid = "backend.datavis",
                 pwd = "Vizsla123",
                 port = 1505)





####################################################################
# creating the task in task-scheduler

myscript <- system.file("extdata", 
                        "C:/Users/hunter5/Desktop/bamosz_daily.R", 
                        package = "taskscheduleR",
                        mustWork = TRUE)
taskscheduler_create(taskname = "bamosz_daily_data_download", 
                     rscript = myscript, 
                     schedule = "DAILY", 
                     starttime = "06:00")





####################################################################
# setting up the web environment before scraping

url <- "https://www.bamosz.hu/letoltesek"

available_versions <- binman::list_versions("chromedriver") 
latest_version = available_versions$win32[length(available_versions$win32)] 

rD <- rsDriver(remoteServerAddr = "localhost", port = 9203L, browser = "chrome", chromever=latest_version)
remDr <- rD$client
remDr$open()
remDr$navigate(url)





####################################################################
# scraping function

bamosz_scraper <- function(date_start, date_end) {
  
  remDr$refresh()
  Sys.sleep(2)
  
  #show inactives
  inactive_checkbox <- remDr$findElement(using = "xpath", "//input[@id='A6035:letoltesForm:showInaktiv']")
  inactive_checkbox$clickElement()
  Sys.sleep(2)
  
  #divider
  sapply(remDr$findElements(using = "xpath", "//label[@class='ui-selectonemenu-label ui-corner-all']"), function(element){ element$clickElement() })
  Sys.sleep(1)
  sapply(remDr$findElements(using = "xpath", "//*[contains(@class, 'ui-selectonemenu-item') and text()='Pontosvessző']"), function(element){ element$clickElement() })
  Sys.sleep(2)
  
  #dates
  sapply(remDr$findElements(using = "xpath", "//input[@id='A6035:letoltesForm:letCal1_input']"), function(element){ 
    element$clearElement()
    Sys.sleep(2)
    element$sendKeysToElement(list(date_start, key = "enter"))
    Sys.sleep(5)
  })
  sapply(remDr$findElements(using = "xpath", "//input[@id='A6035:letoltesForm:letCal2_input']"), function(element){ 
    element$clearElement()
    Sys.sleep(2)
    element$sendKeysToElement(list(date_end, key = "enter"))
    Sys.sleep(5)
  })
  
  #alapok
  alap_box <- remDr$findElement(using = "xpath", "//select[@id='A6035:letoltesForm:alapokList']")
  alap_lista <- alap_box$selectTag()$value %>% as.list()
  for (i in 1:length(alap_lista)) {
    option_value <- paste("//option[@value='", alap_lista[i], "']", sep="")
    sapply(remDr$findElements(using = "xpath", option_value), function(element){ element$clickElement() })
  }
  Sys.sleep(5)
  
  #checkboxes
  sapply(remDr$findElements(using = "xpath", "//div[@class='ui-checkbox-box ui-widget ui-corner-all ui-checkbox-relative ui-state-default']"), function(element){ element$clickElement() })
  Sys.sleep(2)
  
  #submit button
  download_button <- remDr$findElement(using = "id", "A6035:letoltesForm:j_idt120")
  download_button$clickElement()
  Sys.sleep(30)

}



####################################################################
# data structuring function

bamosz_data_structurer <- function(name_of_file) {
  
  raw_df <- read.csv2(name_of_file, header=FALSE, sep=";", dec = ",", 
                      stringsAsFactors=FALSE)
  
  kat_dist <- 15
  raw_df_u <- raw_df[1:kat_dist,]
  raw_df_l <- raw_df[-(1:kat_dist),]
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
    names(temp_df) <- col_names_u #oszlopatnevezes
    if (i==0) {
      categories_df <- temp_df #elso df
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
      col_names_l <- c("Verzio", "Datum", "ISIN_kod", colnames(time_series_df)[-(1:2)])
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
                            "ESG_OSZTALYZAT",
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
  
  #filter out na and arrange
  time_series_df <- mutate(time_series_df, DROP_FLAG = 
                                   is.na(NETTO_ESZKOZERTEK) &
                                   is.na(ARFOLYAM) &
                                   is.na(KIFIZETETT_HOZAMOK) &
                                   is.na(NAPI_BEFJEGY_FORGALOM) &
                                   is.na(REFERENCIAINDEX) &
                                   is.na(EFF_HOZAM_3_HONAPOS)
  )
  time_series_df <- filter(time_series_df, DROP_FLAG == FALSE)
  time_series_df <- select(time_series_df, 1:(length(time_series_df)-1))
  time_series_df <- arrange(time_series_df, VERZIO, ISIN_KOD, DATUM)
  
  
  final_dataframes <- list(categories_df,time_series_df)
  return(final_dataframes)
  
}





####################################################################
# categories table finishing touches

categories_finisher <- function(final_categories_df) {
  
  #rovid nev
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = ALAPKEZELO)
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub('\\(korábbi néven Concorde Alapkezelő\\)', '', ALAPKEZELO_ROVIDNEV) ) 
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Asset Management', '', ALAPKEZELO_ROVIDNEV) )
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Investment Management', '', ALAPKEZELO_ROVIDNEV) )
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Befektetési', '', ALAPKEZELO_ROVIDNEV) )
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Alapkezelő' , '', ALAPKEZELO_ROVIDNEV) )
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Zrt.' , '', ALAPKEZELO_ROVIDNEV) )
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Zrt' , '', ALAPKEZELO_ROVIDNEV) )
  final_categories_df <- mutate(final_categories_df, ALAPKEZELO_ROVIDNEV = gsub(' Rt.', '', ALAPKEZELO_ROVIDNEV) )
  
  #visszarakni a ''-t az alapnevbe                         
  final_categories_df <- mutate(final_categories_df, ALAP_NEVE = gsub('\\?', "\\'", ALAP_NEVE) )
  
  #kozvetett/kozvetlen - ingatlanalap
  final_categories_df <- mutate(final_categories_df, BEFPOL_SZERINTI_KATEGORIA = gsub('Közvetett', 'Közvetett ingatlanalap', BEFPOL_SZERINTI_KATEGORIA) )
  final_categories_df <- mutate(final_categories_df, BEFPOL_SZERINTI_KATEGORIA = gsub('Közvetlen', 'Közvetlen ingatlanalap', BEFPOL_SZERINTI_KATEGORIA) )
  
  return(final_categories_df)
  
}



####################################################################
# create final dates table

date_table_creator <- function(final_time_series_df, curr_date, prev_date) {
  
  #datum es forgalmazasi napok flag
  forg_dates_df <- select(final_time_series_df, "DATUM", "NETTO_ESZKOZERTEK") #datum es NEE
  forg_dates_df[is.na(forg_dates_df)] <- 0
  forg_dates_df <- aggregate(forg_dates_df["NETTO_ESZKOZERTEK"], by=forg_dates_df["DATUM"], sum)
  forg_dates_df <- mutate(forg_dates_df, DATUM = as.Date(DATUM))
  forg_dates_df <- arrange(forg_dates_df, DATUM)
  forg_dates_df <- mutate(forg_dates_df, FORG_NAP_FLAG = ifelse( abs(NETTO_ESZKOZERTEK/lag(NETTO_ESZKOZERTEK)) > 0.2 , 1 , NA)) #igazabol csak pozitiv lehet - abs feles
  forg_dates_df <- select(forg_dates_df, 1, 3) #datum es forg_nap_flag
  forg_dates_df <- subset(forg_dates_df, FORG_NAP_FLAG == 1)
  
  #datumtabla
  dates_vector <- seq(prev_date, curr_date, by = 'days')
  final_dates_df <- data.frame(dates_vector)
  names(final_dates_df) <- c("DATUM")
  
  #forg nap flag oszlop
  final_dates_df <- merge(final_dates_df, forg_dates_df, by="DATUM", all.x=TRUE, sort=TRUE)
  
  #forg datum oszlop
  final_dates_df <- mutate(final_dates_df, FORG_DATUM = as.Date( ifelse(FORG_NAP_FLAG==1, DATUM, NA), origin="1970-01-01" ) )
  final_dates_df$FORG_DATUM <- na.locf( final_dates_df$FORG_DATUM, na.rm = FALSE )
  
  #kimenteni segedtablanak a parokat
  date_pairs_df <- select(final_dates_df, 1, 3)
  names(date_pairs_df) <- c('DATUM_SEGED', 'FORG_DATUM_SEGED' )
  
  
  
  #gordolu datumok
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
  
  
  
  #utolso napok
  
  #het
  fridays <- seq(as.Date("1995-01-06"), curr_date, by = 'weeks')
  fridays <- as.data.frame(fridays)
  names(fridays) <- c("DATUM")
  
  temp_df <- select(final_dates_df, 1, 3)
  temp_df <- mutate(temp_df, DATUM=as.Date(DATUM))
  temp_df <- merge(fridays, temp_df, by="DATUM", all=FALSE, sort=TRUE)
  temp_df <- mutate(temp_df, UNAP_HET=1)  
  temp_df <- select(temp_df, 2:3)
  temp_df <- unique(temp_df) #basszameg, ez volt a hianyzo sor
  temp_df <- mutate(temp_df, FORG_DATUM=as.Date(FORG_DATUM))
  
  final_dates_df <- merge(final_dates_df, temp_df, by.x="DATUM", by.y="FORG_DATUM", all.x=TRUE, sort=TRUE)
  
  #honap - negyedev - ev
  temp_df <- select(final_dates_df, 3)
  temp_df <- mutate(temp_df, HONAP = month(FORG_DATUM) )
  temp_df <- mutate(temp_df, UNAP_HONAP = ifelse( HONAP != lead(HONAP), 1, NA ) )
  temp_df <- filter(temp_df, UNAP_HONAP == 1)
  temp_df <- mutate(temp_df, UNAP_NEGYEDEV = ifelse( HONAP %% 3 == 0, 1, NA ) )
  temp_df <- mutate(temp_df, UNAP_EV = ifelse( HONAP %% 12 == 0, 1, NA ) )
  temp_df <- select(temp_df, 1, 3:5)
  temp_df <- mutate(temp_df, FORG_DATUM=as.Date(FORG_DATUM))
  
  final_dates_df <- merge(final_dates_df, temp_df, by.x="DATUM", by.y="FORG_DATUM", all.x=TRUE, sort=TRUE)
  
  #sort a biztonsag kedveert
  final_dates_df <- arrange(final_dates_df, DATUM)
  
  return(final_dates_df)
  
}





####################################################################
# daily yield calculator for time series df

yield_calculator <- function(final_time_series_df, final_dates_df) {
  
  #hozamszamitas
  final_time_series_df <- merge(final_time_series_df, final_dates_df[,1:2], 
                                by="DATUM", all.x=TRUE, sort=FALSE)
  final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM) #esetleg itt erdemes megnezni mik az NA-k!!!!!
  
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

  
  return(final_time_series_df)
}





####################################################################
# create currency table

currency_table_creator <- function(categories_df, curr_date, prev_date) {

  cob_date <- as.Date(cob_curr_date, format="%Y.%m.%d")
  deviza_list <- categories_df$DEVIZANEM %>% unique
  deviza_list <- deviza_list[deviza_list != "HUF"]
  
  currency_df <- data.frame(matrix(NA, nrow = length(deviza_list), ncol = 3))
  names(currency_df) <- c("DATUM", "DEVIZANEM_BASE", "DEVIZA_ARFOLYAM_BASE")
  currency_df <- mutate(currency_df, DATUM = as.Date(DATUM))
  i=1
  for (i in 1:length(deviza_list)) {
    deviza <- deviza_list[i]
    symbol_HUF <- paste(deviza, "HUF=X", sep="")
    symbol_EUR <- paste("EUR", deviza, "=X", sep="")
    arfolyam <- tryCatch( 
      expr = { getSymbols(symbol_HUF, src = 'yahoo', auto.assign = FALSE, from = prev_date, to = curr_date)[,6] },
      error = function(e){ NA }
    )
    
    if(is.na(arfolyam)) {
      arfolyam <- tryCatch( 
        expr = { getSymbols(symbol_EUR, src = 'yahoo', auto.assign = FALSE, from = prev_date, to = curr_date)[,6] },
        error = function(e){ NA }
      )  
    }
    
    
    deviza <- substr(names(arfolyam[1]), 1, 6)[1]
    names(arfolyam) <- c("adjusted")
    arfolyam$adjusted <- na.locf(arfolyam$adjusted, na.rm=FALSE)
    arfolyam <- drop(coredata( arfolyam[nrow(arfolyam),ncol(arfolyam)] )) 
    
    currency_df[i,1] <- cob_date
    currency_df[i,2] <- deviza
    currency_df[i,3] <- arfolyam
    
  }
  
  
  
  osztando <- as.numeric( filter(currency_df, DEVIZANEM_BASE=="EURHUF")[3] )
  currency_df <- mutate(currency_df, DEVIZANEM = ifelse( substr(DEVIZANEM_BASE,4,6)=="HUF",
                                                         DEVIZANEM_BASE,
                                                         paste(substr(DEVIZANEM_BASE,4,6), "HUF", sep="")) )
  currency_df <- mutate(currency_df, DEVIZA_ARFOLYAM = ifelse( substr(DEVIZANEM_BASE,4,6)=="HUF",
                                                               DEVIZA_ARFOLYAM_BASE,
                                                               osztando/DEVIZA_ARFOLYAM_BASE) )
  
  currency_df <- select(currency_df, 1, 4, 5)
  
  return(currency_df)

}






####################################################################
# exchange everything to HUF

huf_exchanger <- function(final_time_series_df, final_categories_df, final_currency_df) {
  
final_time_series_df <- merge(final_time_series_df, select(final_categories_df, 2, 13), 
                              by="ISIN_KOD", all.x=TRUE, sort=F) #ISIN & DEVIZANEM
final_time_series_df <- merge(final_time_series_df, final_currency_df, 
                              by=c("DATUM", "DEVIZANEM"), all.x=TRUE, sort=F)
final_time_series_df <- mutate(final_time_series_df, NETTO_ESZKOZERTEK_HUF = 
                                 ifelse(DEVIZANEM=="HUF", NETTO_ESZKOZERTEK, NETTO_ESZKOZERTEK*DEVIZA_ARFOLYAM) )
final_time_series_df <- mutate(final_time_series_df, NAPI_BEFJEGY_FORGALOM_HUF = 
                                 ifelse(DEVIZANEM=="HUF", NAPI_BEFJEGY_FORGALOM, NAPI_BEFJEGY_FORGALOM*DEVIZA_ARFOLYAM) )
final_time_series_df <- subset(final_time_series_df, select = -c(DEVIZANEM, DEVIZA_ARFOLYAM))
final_time_series_df <- arrange(final_time_series_df, VERZIO, ISIN_KOD, DATUM)

return(final_time_series_df)

}
