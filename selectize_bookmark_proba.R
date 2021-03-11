# A basic module that captializes text. When bookmarked, it saves the text and a
# hash of the text.
capitalizerUI <- function(id) {
  ns <- NS(id)
  wellPanel(
    h4("Text captializer module"),
    textInput(ns("text"), "Enter text:"),
    "Capitalized text:", 
    verbatimTextOutput(ns("out"))
  )
}
capitalizerServer <- function(input, output, session) {
  output$out <- renderText({
    toupper(input$text)
  })
  onBookmark(function(state) {
    state$values$hash <- digest::digest(input$text, "md5")
  })
  onRestore(function(state) {
    if (identical(digest::digest(input$text, "md5"), state$values$hash)) {
      showNotification(paste0('Module\'s input text "', input$text,
                              '" matches hash ', state$values$hash))
    } else {
      showNotification(paste0('Module\'s input text "', input$text,
                              '" does not match hash ', state$values$hash))
    }
  })
}

bookmark_selectizeGroupServer<- function(input, output, session) {
  
  onBookmark(function(state) {
    state$values$hash <- digest::digest(input[["my-filters-ALAPKEZELO"]], "md5")
  })
}
# The main application calls the module, and also saves its own text and a hash
# of the text.
ui <- function(request) {
  fluidRow(column(4,
    capitalizerUI("tc"),
    h4("Main app"),
    textInput("text", "Enter text:"),
    "Verbatim text:",
    verbatimTextOutput("out"),
    bookmarkButton(id="bookmarkBtn"),
    actionButton("restore", "Szűrők visszaállítása")
  ),
  #selectizegroupUI
  column(8, selectizeGroupUI(
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
  )))
}
server <- function(input, output, session) {
  updateSelectizeInput(
    session,
    inputId = "my-filters-ALAPKEZELO"
  )
  callModule(capitalizerServer, "tc")
  callModule(
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
  
  output$out <- renderText({
    input$text
    
  })
  onBookmark(function(state) {
    state$values$hash <- digest::digest(input[["my-filters-ALAPKEZELO"]], "md5")
    state$values$szuro <- input[["my-filters-LETETKEZELO"]]
    state$values$alapkez <- input[["my-filters-ALAPKEZELO"]]
    # state$values$szuro <- input$text
  })
  
  # onBookmarked(function(url) {
  #   state$values$szuro <- input$text
  # })
  
  # onRestore(function(state) {
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-LETETKEZELO",
  #     selected = c("MKB Bank Nyrt.", "Deutsche Bank ZRt.")
  #   )
  # })
  #bookmarkhoz valtozo  
  vals <- reactiveValues()
  vals_alapk <- reactiveValues()
  # dummy value to initialize
  vals$sum <- NULL
  vals_alapk$sum <- NULL
  
  onRestore(function(state) {
    vals$sum <- state$values$szuro
    vals_alapk$sum <- state$values$alapkez
  })
  
  observeEvent(input$restore, {

  updateSelectizeInput(
    session,
    inputId = "my-filters-LETETKEZELO",
    selected = vals$sum
  )
  updateSelectizeInput(
      session,
      inputId = "my-filters-ALAPKEZELO",
      selected = vals_alapk$sum
    )
  })
  
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })
  
  
  
}
shinyApp(ui, server, enableBookmarking = "url")