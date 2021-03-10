#install.packages("shinythemes")
#install.packages("plotly")
#install.packages("shinyWidgets")
#install.packages("shinythemes")
#install.packages("plotly")
#extrafont::font_import()

# library(shinythemes)
# library(shinyWidgets)
# library(shiny)
# library(dplyr)
# library(ggplot2)
# library(dbplyr)
# library(odbc)

#connection letrehozasa az sql szererrel
# con <- dbConnect(odbc(),
#                 Driver = "SQL Server",
#                 Server = "projectbamosz2.database.windows.net",
#                 Database = "project_bamosz",
#                 UID = "backend.datavis",
#                 PWD = "Vizsla123",
#                 Port = 1433)
#dateframe-be berakom a beimportalt tablakat
<<<<<<< HEAD
categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_data_frame()
currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble() 
dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble() 
timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()
=======

<<<<<<< HEAD
# categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_tibble()
# currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble()
# dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble()
# timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()
=======
#categories_df<- tbl(con, sql("SELECT * FROM dbo.categories_final")) %>% as_tibble() 
#currency_df<- tbl(con, sql("SELECT * FROM dbo.currency_final")) %>% as_tibble() 
#dates_df<- tbl(con, sql("SELECT * FROM dbo.dates_final")) %>% as_tibble() 
#timeseries_df<- tbl(con, sql("SELECT * FROM dbo.timeseries_final")) %>% as_tibble()
>>>>>>> 5fbeffc40b66a8fdd1ed40cb0d6300a95491984e
>>>>>>> c2191785709be85dbd82f057a9495bba0c8ab05c



#ui ######################################
ui <- fluidPage(
  
  navbarPage("Chart builder",
                 tabPanel("Component 1"),
                 tabPanel("Component 2")
  ),
  
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
<<<<<<< HEAD
      uiOutput("AlapNeveout"),
      uiOutput("Alapkezeloout"),
      
      
    ),
    
    
=======
      uiOutput("alapkezelo"),
      uiOutput("alapneve"),
      uiOutput("letetkezelo")
      
    
    ),
   
>>>>>>> c2191785709be85dbd82f057a9495bba0c8ab05c
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
      )
    )
  )
)



shinyApp(ui = ui, server = server)

