#install.packages("shinythemes")
#install.packages("plotly")

library(shinythemes)
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
                 Database = "project_bamosz",   UID = "backend.datavis",
                PWD = "Vizsla123",
                 Port = 1433)
#dateframe-be berakom a beimportalt tablakat
#<<<<<<< HEAD
categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_data_frame()
currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble() 
dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble() 
timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()



#=======

#categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_tibble() 
#currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble() 
#dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble() 
#timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()
#>>>>>>> 5fbeffc40b66a8fdd1ed40cb0d6300a95491984e



#ui ######################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      
      #felteteles panelre kattintva nyissa csak meg a formazas inputokat
     
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
      uiOutput("alapkezelo"),
      uiOutput("alapneve"),
      uiOutput("letetkezelo")
      
    
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
          width=12, offset=1,
          selectInput(
            "formazas_terulet",
            "",
            selected = 'Általános',
            choices = c("Általános",
                        "Színpaletta",
                        "Cím",
                        "X tengely",
                        "Y tengely",
                        "Jelmagyarázat",
                        "Adatfeliratok")
          ),
          conditionalPanel(
            condition = "input.formazas_terulet == 'Színpaletta'",
            column(
              width=12,
                colourpicker::colourInput("col_1", "Szín #1", value = "white"),
                colourpicker::colourInput("col_2", "Szín #2", value = "white"),
                colourpicker::colourInput("col_3", "Szín #3", value = "white"),
                colourpicker::colourInput("col_4", "Szín #4", value = "white"),
                colourpicker::colourInput("col_5", "Szín #5", value = "white"),
                colourpicker::colourInput("col_6", "Szín #6", value = "white")
            )
          ),
          conditionalPanel(
            condition = "input.formazas_terulet == 'Általános'",
            column(
              width=12,
                colourpicker::colourInput("base_col", "Háttér szín", value = "white"),
                selectInput(
                  "font_type",
                  "Betűtípus",
                  selected = 'F',
                  choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
                )
            )
          ),
          conditionalPanel(
            condition = "input.formazas_terulet == 'Cím'",
            column(
              width=12,
                textInput(
                  'title_text',
                  'Cím megadása:',
                  value="Cím"
                ),
                numericInput("title_size", "Cím mérete:", "22"),
                colourpicker::colourInput("title_color", "Cím színe", value = "black"),
                selectInput(
                  "title_alignment",
                  "Cím igazítása",
                  selected = 'F',
                  choices = c(0, 0.5, 1)
                )
            )
          ),
          conditionalPanel(
            condition = "input.formazas_terulet == 'X tengely'",
            column(
              width=12,
                textInput(
                  'x_text',
                  'Tengelycím (x) megadása:',
                  value="Cím"
                ),
                numericInput("x_size", "Tengelycím (x) mérete:", "12"),
                colourpicker::colourInput("x_color", "Tengelycím (x) színe", value = "black"),
                selectInput(
                  "x_alignment",
                  "Tengelycím (x) igazítása",
                  selected = 'F',
                  choices = c(0, 0.5, 1)
                )
            )
          )
        )
      )
    )))



shinyApp(ui = ui, server = server)

