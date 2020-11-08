library(shiny)
library(shinyWidgets)


data("mpg", package = "ggplot2")

ui <- fluidPage(fluidRow(
  column(
    width = 4,
    tags$h3("Filter data with selectize group"),
    dateRangeInput('dateRange',
                   label = 'Datum kivalasztasa',
                   start = Sys.Date() - 200, end = Sys.Date() - 150
    ),
    #kategoria, ertek kivalasztasa
    varSelectInput(
      "kategoriavalaszt",
      "Kategoria:",
      categories_df,
      multiple = FALSE,
      selected = "ALAP_NEVE"
    ),
    varSelectInput(
      "valuevalaszt",
      "Ertekvalasztas",
      timeseries_df,
      multiple = FALSE,
      selected = "NETTO_ESZKOZERTEK"
    ),
    panel(
      selectizeGroupUI(
        id = "my-filters",
        inline = FALSE,
        params = list(
          ALAPKEZELO = list(inputId = "ALAPKEZELO", title = "Alapkezelő:"),
          ALAP_NEVE = list(inputId = "ALAP_NEVE", title = "Alap neve:"),
          BEFPOL_SZERINTI_KATEGORIA = list(inputId = "BEFPOL_SZERINTI_KATEGORIA", title = "Befektetési politika:"),
          LETETKEZELO = list(inputId = "LETETKEZELO", title = "Letétkezelő"),
          ALAPTIPUS = list(inputId = "ALAPTIPUS", title = "Alapítpus:"),
          ALAPFAJTA = list(inputId = "ALAPFAJTA", title = "Alapfajta:"),
          DEVIZALIS_KITETTSEG = list(inputId = "DEVIZALIS_KITETTSEG", title = "Devizális kitettség:"),
          FOLDRAJZI_KITETTSEG = list(inputId = "FOLDRAJZI_KITETTSEG", title = "Földrajzi kitettség:"),
          EGYEB_KITETTSEG = list(inputId = "EGYEB_KITETTSEG", title = "Egyéb kitettség:"),
          DEVIZANEM = list(inputId = "DEVIZANEM", title = "Devizanem:"),
          STATUSZ = list(inputId = "STATUSZ", title = "Státusz:")
        )
      ),
      status = "primary"
    ),
     
    
    
    
  ),
  
  column(width = 8, dataTableOutput(outputId = "table"))
  
))


server <- function(input, output, session) {
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = categories_df,
    vars = c(
      "ALAPKEZELO",
      "ALAP_NEVE",
      "LETETKEZELO",
      "BEFPOL_SZERINTI_KATEGORIA",
      "LETETKEZELO",
      "ALAPTIPUS",
      "ALAPFAJTA",
      "DEVIZALIS_KITETTSEG",
      "FOLDRAJZI_KITETTSEG",
      "EGYEB_KITETTSEG",
      "DEVIZANEM",
      "STATUSZ"
    )
  )

#beadom kulon reaktiv valtozoba a lekerdezett timeseries_df-et, es így mar csak akkor kell ujratoltenie, ha datumban van valtozas
  
  react_ts <- reactive({
  sqldf::sqldf(
    paste(
      "select * FROM timeseries_df WHERE DATUM >'",
      input$dateRange[1],
      "' AND DATUM<", "'",
      input$dateRange[2], "'",
      "",
      sep = ""
    )
  )
})

react_selected <- reactive({
  react_ts() %>% select(ISIN_KOD, !!!input$valuevalaszt)
})
  
#mergelem a lekerdezett reactive time-series df valtozot a categories-zal
  output$table <-
    renderDataTable(merge(
      res_mod() %>% select(ISIN_KOD,!!!input$kategoriavalaszt),
      react_selected(),
      by.x = "ISIN_KOD",
      by.y = "ISIN_KOD"
    ) %>% select(!ISIN_KOD))
  

 
  
}

shinyApp(ui, server)