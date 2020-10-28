install.packages("shinythemes")
install.packages("plotly")

library(shiny)
library(dplyr)
library(ggplot2)
library(dbplyr)
#install.packages("shinythemes")
#install.packages("plotly")
#extrafont::font_import()
#library(odbc)
#connection letrehozasa az sql szererrel
#con <- dbConnect(odbc(),
 #                Driver = "SQL Server",
  #               Server = "projectbamosz2.database.windows.net",
   #              Database = "project_bamosz",
    #             UID = "backend.datavis",
     #            PWD = "Vizsla123",
      #           Port = 1433)

#categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_tibble() 
#currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble() 
#dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble() 
#timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()

#aktiv alapokra szûrés
#aktiv_isin <- categories_df %>% filter(STATUSZ=="Aktív") %>% select(ISIN_KOD) %>% unique()
#timeseries_active <- timeseries_df %>% filter(ISIN_KOD %in% aktiv_isin)

#import_data
alapok_df<- read.csv("C:/Users/Domonkos/Desktop/bamosz/data_shiny.csv", encoding="UTF-8", stringsAsFactors=FALSE)
names(alapok_df)[1] <- "Datum"
alapok_df$Datum <- alapok_df$Datum %>% as.Date
alapok_df$EFF_HOZAM_1_EVES = readr::parse_number(alapok_df$EFF_HOZAM_1_EVES)/100
alapok_df$EFF_HOZAM_3_EVES = readr::parse_number(alapok_df$EFF_HOZAM_3_EVES)/100


#ui ######################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #felteteles panelre kattintva nyissa csak meg a formazas inputokat
      selectizeInput(
        'AlapNeve',
        'Alap neve',
        choices = alapok_df$ALAP_NEVE,
        selected = alapok_df$ALAP_NEVE[1],
        multiple = TRUE
      ),
      shinythemes::themeSelector(),
      selectInput(
        "valueselect",
        "Érték kiválasztása",
        selected = 'ARFOLYAM',
        choices = c("ARFOLYAM", "EFF_HOZAM_1_EVES", "EFF_HOZAM_3_EVES")
      ),
      
      selectInput(
        "categoryselect",
        "Kategória kiválasztása",
        selected = 'Datum',
        choices = c(
          "Datum",
          "ALAP_NEVE",
          "ALAPKEZELO",
          "EFF_HOZAM_3_EVES",
          "BEFPOL_SZERINTI_KATEGORIA",
          "ISIN_KOD"
        )
      ),
    ),
   
    
    mainPanel(
      # egy panelbe rakom line chartot, table-t (felso savon lehet valtani)
      tabsetPanel(
        #line chart
        tabPanel('Line chart', plotly::plotlyOutput('Alap_arfolyama_plot')),
        #table
        tabPanel('Table', DT::DTOutput('Alap_arfolyama_table')),
        #scatter
        tabPanel('Scatter plot', plotly::plotlyOutput('Alap_arfolyama_Scatterplot')),
        #bar
        tabPanel('Bar chart', plotly::plotlyOutput('Alap_arfolyama_colplot')),
        #ribbon
        tabPanel('Area chart', plotly::plotlyOutput('Alap_arfolyama_areaplot'))
        
      ),
      selectInput(
        "formazas",
        "Formázás",
        selected = 'Default',
        choices = c("Default", "Egyedi")
      ),
      conditionalPanel(
        condition = "input.formazas == 'Egyedi'",
        column(
          2,
          actionButton(inputId = "generalButton", label = "Általános"),
          conditionalPanel(
            condition = ("input.generalButton%2!=0"),
            colourpicker::colourInput("col", "Háttérszín", value = "grey"),
            textInput(
              'AbraCime',
              'Cím megadása:',
              value="Cím megadása"
            ),
            numericInput("Cimsize", "Cím mérete:", "22")
          )
        ),
        column(
          2,
          actionButton(inputId = "xButton", label = "X tengely"),conditionalPanel(condition = ("input.xButton%2!=0"),
          selectInput(
            "betutipus",
            "Betûtípus",
            selected = 'F',
            choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
          ))
        ),
        column(
          2,
          actionButton(inputId = "yButton", label = "Y tengely"),conditionalPanel(condition = ("input.yButton%2!=0"),
                                                                                  selectInput(
                                                                                    "betutipus",
                                                                                    "Betûtípus",
                                                                                    selected = 'F',
                                                                                    choices = c("Times", "Calibri", "Helvetica")
                                                                                  ))
        ),
        column(
          2,
          actionButton(inputId = "jelmagyButton", label = "Jelmagyarázat"),conditionalPanel(condition = ("input.jelmagyButton%2!=0"),
                                                                                  selectInput(
                                                                                    "betutipus",
                                                                                    "Betûtípus",
                                                                                    selected = 'F',
                                                                                    choices = c("Times", "Calibri", "Helvetica")
                                                                                  ))
        ),
        column(
          2,
          actionButton(inputId = "adatfelButton", label = "Adatfeliratok és színek"),conditionalPanel(condition = ("input.adatfelButton%2!=0"),
                                                                                            selectInput(
                                                                                              "betutipus",
                                                                                              "Betûtípus",
                                                                                              selected = 'F',
                                                                                              choices = c("Times", "Calibri", "Helvetica")
                                                                                            ))
        ),
        
        
      )
    )))



shinyApp(ui = ui, server = server)

