library(DBI)
library(dbplyr)
library(dplyr)
library(odbc)

con <- dbConnect(drv = odbc::odbc(),
                 Driver = "SQL Server", 
                 Server = "projectbamosz2.database.windows.net", 
                 Database = "project_bamosz ",
                 uid = "backend.datavis",
                 pwd = "Vizsla123",
                 port = 1506)

dates_df <- dbReadTable(con, "dates_final")
timeseries_df <- dbReadTable(con, "timeseries_final")
categories_df <- dbReadTable(con, "categories_final")
currency_df <- dbReadTable(con, "currency_final")




dbWriteTable(con, "mtcars", mtcars[1:3,], append=T)
temp_df <- dbReadTable(con, "mtcars")

sql_string <- "DELETE FROM mtcars WHERE row_names LIKE 'Mazda RX4%';"
dbSendStatement(con, sql_string)

