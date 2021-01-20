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
                 port = 1505)


dates_df <- dbReadTable(con, "dates_final")
timeseries_df <- dbReadTable(con, "timeseries_final")
categories_df <- dbReadTable(con, "categories_final")
currency_df <- dbReadTable(con, "currency_final")

# dates_df <- dates_final
# timeseries_df <- timeseries_final
# categories_df <- categories_final
# currency_df <- currency_final




base_data %>%
  
  ggplot(aes(
    x = hp,
    y = sum(cyl) ,
    label = sum(cyl),
    color= mpg,
    shape = factor(cyl),
    fill = mpg
  )) +
  geom_col() + 
  
  ggtitle("input$title_text") +
  xlab("input$x_text") +
  ylab("input$y_text") +
  geom_text(nudge_y = 1, color = "black") +
  
  theme(legend.position = "left")

