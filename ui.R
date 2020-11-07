#install.packages("shinythemes")
#install.packages("plotly")

library(shiny)
library(dplyr)
library(ggplot2)
library(dbplyr)
#install.packages("shinythemes")
#install.packages("plotly")
#extrafont::font_import()
library(odbc)
#connection letrehozasa az sql szererrel
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "projectbamosz2.database.windows.net",
                 Database = "project_bamosz",
                 UID = "backend.datavis",
                 PWD = "Vizsla123",
                 Port = 1433)
#dateframe-be berakom a beimportalt tablakat
categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_tibble() 
currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble() 
dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble() 
timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()


#ui ######################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #felteteles panelre kattintva nyissa csak meg a formazas inputokat
      selectizeInput(
        'AlapNeve',
        'Alap neve',
        choices = categories_df$ALAP_NEVE,
        selected = categories_df$ALAP_NEVE[1],
        multiple = TRUE
      ),
      shinythemes::themeSelector(),
      #ez az érték szuro (milyen értékek szerepeljenek a ggplot-on)
      selectInput(
        "valueselect",
        "ertek kivalasztasa",
        selected = 'ARFOLYAM',
        choices = colnames(timeseries_df)
      ),
      
      #ez a kategoria szuro (mi szerint rendezzen ggplot-on)
      selectInput(
        "categoryselect",
        "kategoria kivalasztasa",
        selected = 'Datum',
        #a felhasznalo kivalaszthatja a categories_df tablazat, osszes oszlopat, kiveve verzio, meg alapkezelo_rovidnev
        choices = categories_df %>% select(!ALAPKEZELO_ROVIDNEV) %>% select(!VERZIO) %>% colnames
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
          actionButton(inputId = "generalButton", label = "?ltal?nos"),
          conditionalPanel(
            condition = ("input.generalButton%2!=0"),
            colourpicker::colourInput("col", "Háttér szín", value = "grey"),
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
            "Bet?t?pus",
            selected = 'F',
            choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
          ))
        ),
        column(
          2,
          actionButton(inputId = "yButton", label = "Y tengely"),conditionalPanel(condition = ("input.yButton%2!=0"),
                                                                                  selectInput(
                                                                                    "betutipus",
                                                                                    "Bet?t?pus",
                                                                                    selected = 'F',
                                                                                    choices = c("Times", "Calibri", "Helvetica")
                                                                                  ))
        ),
        column(
          2,
          actionButton(inputId = "jelmagyButton", label = "Jelmagyarázat"),conditionalPanel(condition = ("input.jelmagyButton%2!=0"),
                                                                                  selectInput(
                                                                                    "betutipus",
                                                                                    "Bet?t?pus",
                                                                                    selected = 'F',
                                                                                    choices = c("Times", "Calibri", "Helvetica")
                                                                                  ))
        ),
        column(
          2,
          actionButton(inputId = "adatfelButton", label = "Adatfeliratok és színek"),conditionalPanel(condition = ("input.adatfelButton%2!=0"),
                                                                                            selectInput(
                                                                                              "betutipus",
                                                                                              "Bet?t?pus",
                                                                                              selected = 'F',
                                                                                              choices = c("Times", "Calibri", "Helvetica")
                                                                                            ))
        ),
        
        
      )
    )))



shinyApp(ui = ui, server = server)

