library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(httr)
library(DBI)
library(dbplyr)
library(dplyr)
library(odbc)
library(plotly)
library(httr)
library(shinydashboard)
library(shinydashboardPlus)
library(treemapify)
library(dbplyr)
library(sqldf)
library(DT)
library(colourpicker)
library(rmarkdown)



ui <-
  function(request) {
    dashboardPagePlus(
      collapse_sidebar = TRUE,
      header = dashboardHeaderPlus(
        fixed = FALSE,
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "cog"
        # left_menu =
        #   tagList(
        #     dropdownBlock(
        #       id = "dropdown_title",
        #       title = "Cím",
        #       icon = icon("sliders")
        #     )
        #   )
      ),
      
      sidebar = dashboardSidebar(collapsed =
                                   TRUE,
                                 
                                 sidebarMenu(
                                   menuItem("Szűrők", tabName = "szurok", icon = icon("filter")),
                                   menuItem(
                                     "Diagram-elemek",
                                     tabName = "diagram",
                                     icon = icon("area-chart")
                                   ),
                                   menuItem(
                                     "Színezés",
                                     tabName = "szinezes",
                                     icon = icon("paint-brush")
                                   )
                                   
                                 )),
      
      body = dashboardBody(
        style = "background-color:#444444;",
        setShadow(class = "dropdown-menu"),
        
        fluidRow(
          #tags$style(type='text/css', ".selectize-dropdown-content {max-height: 1000px !important; }"),
          column(
            4,
            bookmarkButton(id = "bookmarkBtn"),
            textInput(
              inputId = "bookmarkcim",
              label = "Mentett riport neve",
              placeholder = "Data Summary"
            ),
            actionButton("restore", "Mentett szűrők betöltése"),
            tabItems(
              tabItem(
                tabName = "szurok",
                h2("Szűrők"),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Dátumszűrő",
                  status = "warning",
                  solidHeader = T,
                  dateRangeInput(
                    'dateRange',
                    label = 'Dátum kivalasztasa',
                    start = "2020-08-03",
                    end = "2020-08-05"
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Tengely- és értékszűrők",
                  status = "warning",
                  solidHeader = T,
                  #kategoria, ertek kivalasztasa
                  varSelectInput(
                    "tengelyvalaszt",
                    "Tengely:",
                    merge(categories_df, timeseries_df %>% select(ISIN_KOD, DATUM)),
                    multiple = FALSE,
                    selected = "ALAPKEZELO_ROVIDNEV"
                  ),
                  
                  #datum havi heti stb. illetve datum aggregalva legyen-e?
                  conditionalPanel(
                    "input.tengelyvalaszt=='DATUM'",
                    selectInput(
                      "datumszuro",
                      "datumszuro",
                      c("UNAP_EV", "UNAP_NEGYEDEV", "UNAP_HONAP", "UNAP_HET", "Napi"),
                      selected = "Napi"
                    ),
                  ),
                  
                  #switch: aggergalva legyen-e
                  conditionalPanel(
                    "input.tengelyvalaszt=='DATUM'",
                    materialSwitch(
                      inputId = "datumagr",
                      label = "Időben összevonva",
                      status = "primary"
                    )
                  ),
                  
                  #kategoria, ertek kivalasztasa
                  varSelectInput(
                    "kategoriavalaszt",
                    "Jelmagyarázat:",
                    categories_df,
                    multiple = FALSE,
                    selected = "DEVIZANEM"
                  ),
                  
                  varSelectInput(
                    "valuevalaszt",
                    "Értékek",
                    timeseries_df,
                    multiple = FALSE,
                    selected = "NETTO_ESZKOZERTEK"
                  ),
                  
                  selectInput(
                    "osszegzofv",
                    "Összegző függvény",
                    c("mean", "sum", "count", "max", "min"),
                    selected = "sum"
                  )
                  
                  
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Kategóriaszűrők",
                  status = "warning",
                  solidHeader = T,
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
                        STATUSZ = list(inputId = "STATUSZ", title = "Státusz:"),
                        options =
                          list(maxOptions = 20000)
                      )
                    ),
                    status = "warning"
                  )
                  
                )
                
              ),
              
              
              tabItem(
                tabName = "diagram",
                h2("Diagram-elemek"),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Vászon",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(column(
                    width = 12,
                    selectInput(
                      "font_type",
                      "Betűtípus",
                      selected = 'F',
                      choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
                    ),
                    colourpicker::colourInput("col_base", "Háttér szín", value = "white")
                  )),
                  fluidRow(
                    column(
                      width = 6,
                      checkboxInput(
                        "xgrid",
                        "X tengely rácsvonalak",
                        value = F,
                        width = NULL
                      ),
                      conditionalPanel(
                        condition = "input.xgrid > 0",
                        colourpicker::colourInput("xgrid_color", "Rácsvonal színe", value = "white")
                      )
                    ),
                    column(
                      width = 6,
                      checkboxInput(
                        "ygrid",
                        "Y tengely rácsvonalak",
                        value = F,
                        width = NULL
                      ),
                      conditionalPanel(
                        condition = "input.ygrid > 0",
                        colourpicker::colourInput("ygrid_color", "Rácsvonal színe", value = "white")
                      )
                    )
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Cím",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(column(
                    width = 12,
                    textInput('title_text', 'Cím megadása:', value = "Default cím")
                  )),
                  fluidRow(
                    column(
                      width = 6,
                      radioGroupButtons(
                        inputId = "title_align",
                        label = "Cím igazítása:",
                        choiceNames = list(
                          icon("align-left"),
                          icon("align-center"),
                          icon("align-right")
                        ),
                        choiceValues = list(0, 0.5, 1),
                        justified = TRUE,
                        status = "primary"
                      ),
                      checkboxGroupButtons(
                        inputId = "title_font",
                        label = "Cím kiemelése:",
                        choiceNames = list(icon("bold"), icon("italic")),
                        choiceValues = list("bold", "italic"),
                        justified = TRUE,
                        status = "primary"
                      )
                    ),
                    column(
                      width = 6,
                      numericInput("title_size", "Cím betűmérete:", "22"),
                      colourpicker::colourInput("title_color", "Cím színe:", value = "black")
                    )
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Jelmagyarázat",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(column(
                    width = 12,
                    radioGroupButtons(
                      inputId = "legend_align",
                      label = "Jelmagyarázat helye:",
                      choiceNames = list(icon("ban"),
                                         icon("arrow-right"),
                                         icon("arrow-down")),
                      choiceValues = list("none", "right", "bottom"),
                      justified = TRUE,
                      status = "primary"
                    )
                  )),
                  fluidRow(
                    conditionalPanel(
                      condition = "input.legend_align !== 'none'",
                      column(
                        width = 6,
                        h4("Jelm. címe"),
                        textInput('legend_titletext', 'Cím megadása:', value = "Jelmagyarázat"),
                        numericInput("legend_titlesize", "Betűmérete:", "12"),
                        colourpicker::colourInput("legend_titlecolor", "Betűszíne:", value = "black")
                      ),
                      column(
                        width = 6,
                        h4("Jelm. szövege"),
                        #numericInput("legend_markersize", "Marker mérete:", "2"),
                        numericInput("legend_textsize", "Betűmérete:", "9"),
                        colourpicker::colourInput("legend_textcolor", "Betűszíne:", value = "black")
                      )
                    ),
                    column(
                      width = 12,
                      conditionalPanel(
                        condition = "input.legend_align == 'bottom'",
                        numericInput(
                          "legend_adjustment",
                          "Függőleges kiigazítás:",
                          value = 0.3,
                          min = 0,
                          max = 1,
                          step = 0.05
                        )
                      )
                    )
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "X tengely",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(
                    column(
                      style = 'border-right: 1px solid grey',
                      width =
                        6,
                      h4("Tengelyfelirat"),
                      textInput('x_text',
                                'Felirat szövege:',
                                value = "Default x tengely"),
                      checkboxGroupButtons(
                        inputId = "x_font",
                        label = "Kiemelése:",
                        choiceNames = list(icon("bold"), icon("italic")),
                        choiceValues = list("bold", "italic"),
                        justified = TRUE,
                        status = "primary"
                      ),
                      numericInput("x_size", "Betűmérete:", "12"),
                      colourpicker::colourInput("x_color", "Színe", value = "black")
                    ),
                    column(
                      width = 6,
                      h4("Tengely/skála"),
                      sliderInput(
                        "xscale_angle",
                        label = "Elforgatás",
                        min = -180,
                        max = 180,
                        value = 0,
                        step = 15,
                        ticks = F,
                        round = T,
                        post = "°"
                      ),
                      numericInput("xscale_size", "Betűmérete:", "12"),
                      colourpicker::colourInput("xscale_fontcolor", "Színe", value = "black"),
                      br(),
                      numericInput("xscale_width", "Tengelyvonal vastagsága:", "1"),
                      colourpicker::colourInput("xscale_color", "Tengelyvonal színe", value = "dimgray"),
                      checkboxInput(
                        "xscale_tick",
                        "Osztásközök mutatása",
                        value = T,
                        width = NULL
                      )
                    )
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Y tengely",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(
                    column(
                      style = 'border-right: 1px solid grey',
                      width =
                        6,
                      h4("Tengelyfelirat"),
                      textInput('y_text',
                                'Felirat szövege:',
                                value = "Default y tengely"),
                      checkboxGroupButtons(
                        inputId = "y_font",
                        label = "Kiemelése:",
                        choiceNames = list(icon("bold"), icon("italic")),
                        choiceValues = list("bold", "italic"),
                        justified = TRUE,
                        status = "primary"
                      ),
                      numericInput("y_size", "Betűmérete:", "12"),
                      colourpicker::colourInput("y_color", "Színe", value = "black")
                    ),
                    column(
                      width = 6,
                      h4("Tengely/skála"),
                      sliderInput(
                        "yscale_angle",
                        label = "Elforgatás",
                        min = -180,
                        max = 180,
                        value = 0,
                        step = 15,
                        ticks = F,
                        round = T,
                        post = "°"
                      ),
                      numericInput("yscale_size", "Betűmérete:", "12"),
                      colourpicker::colourInput("yscale_fontcolor", "Színe", value = "black"),
                      br(),
                      numericInput("yscale_width", "Tengelyvonal vastagsága:", "1"),
                      colourpicker::colourInput("yscale_color", "Tengelyvonal színe", value = "dimgray"),
                      checkboxInput(
                        "yscale_tick",
                        "Osztásközök mutatása",
                        value = T,
                        width = NULL
                      )
                    )
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Adatfeliratok",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(
                    column(
                      width = 6,
                      checkboxGroupButtons(
                        inputId = "label_font",
                        label = "Kiemelése:",
                        choiceNames = list(icon("bold"), icon("italic")),
                        choiceValues = list("bold", "italic"),
                        justified = TRUE,
                        status = "primary"
                      ),
                      numericInput("label_size", "Betűmérete:", "3"),
                      colourpicker::colourInput("label_color", "Színe", value = "black")
                    )
                  )
                )
                
                
                
                
              ),
              
              
              tabItem(
                tabName = "szinezes",
                h2("Színek"),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Alapvető színek",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(
                    column(
                      width = 6,
                      colourpicker::colourInput("col_1", "Szín #1", value = "red"),
                      colourpicker::colourInput("col_2", "Szín #2", value = "blue"),
                      colourpicker::colourInput("col_3", "Szín #3", value = "orange"),
                      colourpicker::colourInput("col_4", "Szín #4", value = "yellow"),
                      colourpicker::colourInput("col_5", "Szín #5", value = "brown"),
                      colourpicker::colourInput("col_6", "Szín #6", value = "black")
                    ),
                    column(
                      width = 6,
                      colourpicker::colourInput("col_7", "Szín #7", value = "grey"),
                      colourpicker::colourInput("col_8", "Szín #8", value = "green"),
                      colourpicker::colourInput("col_9", "Szín #9", value = "purple"),
                      colourpicker::colourInput("col_10", "Szín #10", value = "pink"),
                      colourpicker::colourInput("col_11", "Szín #11", value = "white"),
                      colourpicker::colourInput("col_12", "Szín #12", value = "white")
                    )
                  )
                ),
                
                boxPlus(
                  width = 12,
                  collapsed = T,
                  collapsible = T,
                  closable = F,
                  title = "Feltételes formázás",
                  status = "warning",
                  solidHeader = T,
                  fluidRow(
                    column(
                      width = 6,
                      colourpicker::colourInput("col_felt_1", "Szín +", value = "green")
                    ),
                    column(
                      width = 6,
                      colourpicker::colourInput("col_felt_2", "Szín -", value = "red")
                    )
                  )
                )
              )
              
              
              
              
              
              
            )
            
            
          ),
          
          column(
            8,
            
            #plotly::plotlyOutput('Alap_arfolyama_colplot'),
            
            boxPlus(
              width = 12,
              status = "danger",
              
              tabsetPanel(
                id = "chart_tabset",
                #line chart
                tabPanel('Táblázat', DT::DTOutput("tabletab")),
                tabPanel('Halmozott oszlop', plotly::plotlyOutput('barplot')),
                tabPanel(
                  'Csoportosított oszlop',
                  plotly::plotlyOutput('barplotclust')
                ),
                tabPanel('Halmozott sávdiagram', plotly::plotlyOutput('savplot')),
                tabPanel(
                  'Csoportosított sávdiagram',
                  plotly::plotlyOutput('savplotclust')
                ),
                tabPanel('Pontdiagram', plotly::plotlyOutput('pointplot')),
                tabPanel('Területdiagram', plotly::plotlyOutput('areaplot')),
                tabPanel('Vonaldiagram', plotly::plotlyOutput('lineplot')),
                tabPanel('Kördiagram', plotly::plotlyOutput('pieplot')),
                tabPanel('Fánkdiagram', plotly::plotlyOutput('donutplot')),
                tabPanel('Treemap', plotOutput('treemapplot'))
                
              )
              
            ),
            textOutput("text"),
            textOutput("text2"),
            actionButton("save_plot_as_image_button", "Mentés képként"),
            downloadButton('downloadPlot', 'Download Plot')
          )
          
          
        )
        
        
      ),
      
      
      rightsidebar =
        rightSidebar(
          background = "dark",
          #width = 300,
          rightSidebarTabContent(
            id = 1,
            title = "Dátum",
            active = TRUE,
            icon = "calendar",
            
            
            
          ),
          
          rightSidebarTabContent(id = 2, title = "Érték és kategória",),
          rightSidebarTabContent(id = 3, title = "Szűrők",)
        ),
      
      title = "Formázás",
      skin = "yellow"
    )
    
    
    
  }



#shinyApp(ui = ui, server = server)



# title = 'Cím',
# textInput('title_text',
#           'Cím megadása:',
#           value = "Cím"),
# prettySwitch(
#   inputId = "title_switch",
#   label = "Részletes beállítások",
#   fill = TRUE,
#   status = "primary"
# ),
# conditionalPanel(
#   condition = "input.title_switch > 0",
#   radioGroupButtons(
#     inputId = "t",
#     label = "Choices",
#     choiceNames = list( icon("align-left"), icon("align-center"), icon("align-right") ),
#     choiceValues = list(0, 0.5, 1),
#     justified = TRUE,
#     status = "primary"
#   ),
#   radioButtons(
#     inputId = "title_alignment",
#     label = "Choices",
#     choiceNames = list( icon("align-left"), icon("align-center"), icon("align-right") ),
#     choiceValues = list(0, 0.5, 1)
#   ),
#   numericInput("title_size", "Cím mérete:", "22"),
#   colourpicker::colourInput("title_color", "Cím színe", value = "black")
# )

library(shiny)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(plotly)
library(httr)
library(DBI)
library(dbplyr)
library(dplyr)
library(odbc)
library(plotly)
library(httr)
library(shinydashboard)
library(shinydashboardPlus)
library(treemapify)
library(dbplyr)
library(sqldf)
library(DT)
library(colourpicker)
#options(shiny.sanitize.errors = FALSE)


#szurok alkalmazasa categories tablan
server <- function(input, output, session) {
  #bookmarkhoz valtozo
  vals <- reactiveValues()
  vals_alapNev <- reactiveValues()
  vals_alapkez <- reactiveValues()
  vals_befpol <- reactiveValues()
  vals_letetkez <- reactiveValues()
  vals_alaptipus <- reactiveValues()
  vals_alapfajta <- reactiveValues()
  vals_devizalis <- reactiveValues()
  vals_foldrajzi <- reactiveValues()
  vals_egyeb <- reactiveValues()
  vals_devizanem <- reactiveValues()
  vals_status <- reactiveValues()
  
  # dummy value to initialize
  vals$sum <- NULL
  vals_alapNev$sum <- NULL
  vals_alapkez$sum <- NULL
  vals_befpol$sum <- NULL
  vals_letetkez$sum <- NULL
  vals_alaptipus$sum <- NULL
  vals_alapfajta$sum <- NULL
  vals_devizalis$sum <- NULL
  vals_foldrajzi$sum <- NULL
  vals_egyeb$sum <- NULL
  vals_devizanem$sum <- NULL
  vals_status$sum <- NULL
  
  
  
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
  
  
  #beadom kulon reaktiv valtozoba a datumra szurt timeseries_df-et, es
  #így mar csak akkor kell ujratoltenie, ha datumban van valtozas
  
  react_ts <- reactive({
    sqldf::sqldf(
      paste(
        "select * FROM timeseries_df WHERE DATUM >='",
        input$dateRange[1],
        "' AND DATUM<=",
        "'",
        input$dateRange[2],
        "'",
        "",
        sep = ""
      )
    )
  })
  
  
  # leszurt ISINek kivalasztasa a datumra leszurt timeseries tablabol
  react_isin_szurt <- reactive({
    react_ts() %>% filter(ISIN_KOD %in% res_mod()$ISIN_KOD)
  })
  
  #ISIN kod, kivalasztott timeseries ertek kivalasztasa egy tablaba, majd ezt fogom mergelni a categories_df-hez
  react_selected <- reactive({
    react_isin_szurt() %>% select(ISIN_KOD,!!!input$valuevalaszt, DATUM)
  })
  
  #leszurom a categories tablat
  react_categories_szurt_ISIN <- reactive({
    categories_df %>% filter(ISIN_KOD %in% res_mod()$ISIN_KOD)
  })
  
  #########################kerdes: csinalhatnam azt hogy az elejen mergelem az egeszet, igy utolag mar nem kell uj mergelest csinalni csak selectet
  
  #merge abra_df (mergelem a kategoria szukseges oszlopat a timeseries oszlopokkal),
  #ha datum a tengely, akkor itt ne válassza ki mégegyszer, mert akkro egy uresre kicserelne a mar kivalasztott datum oszlopot, ezert van az ifelse elagazas
  abra_df_react_dates_df_nelkul <- reactive(
    merge(
      if (input$tengelyvalaszt == "DATUM") {
        res_mod() %>% select(ISIN_KOD, !!!input$kategoriavalaszt)
      } else {
        res_mod() %>% select(ISIN_KOD,
                             !!!input$kategoriavalaszt,
                             !!!input$tengelyvalaszt)
      },
      react_selected(),
      by.x = "ISIN_KOD",
      by.y = "ISIN_KOD"
    ) %>% select(!ISIN_KOD)
  )
  
  
  
  #hozzaadom a datum oszlopot (ha napit ad hozzá, akor csak siman a datumot adom hozza, kuonben meg azt az oszlopot amit kivalaszt: utolso het, utolso honap stb)
  abra_df_react_non_grouped <- reactive({
    if (input$datumszuro != "Napi") {
      merge(
        abra_df_react_dates_df_nelkul(),
        dates_df %>%
          select(DATUM, which(
            colnames(dates_df) == input$datumszuro
          )),
        by = "DATUM",
        all.x = TRUE,
        all.y = FALSE
      ) %>%
        filter(get(colnames(dates_df)[which(colnames(dates_df) == input$datumszuro)]) ==
                 1)
    } else {
      abra_df_react_dates_df_nelkul()
    }
  })
  
  #fuggveny, ami csinal group by tablat, amit majd összegzo fuggveny input alapjan hasznalunk
  group_data <- function(df, y, x, fill, fv) {
    fv <- get(fv)
    df2 <- as.data.frame(
      df %>% select(y, x, fill) %>%
        group_by(get(x), get(fill)) %>%
        dplyr::summarise(sumArfolyam = fv(get(y)))
    )
    df2 <- as.data.frame(df2)
    colnames(df2) <- if (x == fill) {
      c(x, paste(x, "2"), y)
    } else {
      c(x, fill, y)
    }
    
    return(df2)
  }
  
  #alkalmazom a group_data fuggvenyt
  abra_df_react <-
    reactive({
      group_data(
        abra_df_react_non_grouped(),
        as.character(input$valuevalaszt),
        as.character(input$tengelyvalaszt),
        as.character(input$kategoriavalaszt),
        fv = input$osszegzofv
      )
    })
  
  #mergelem a lekerdezett reactive time-series df valtozot a categories-zal
  output$tabletab <-
    #DT::renderDT(abra_df_react())
    DT::renderDT(
      DT::datatable(
        abra_df_react(),
        selection = "none",
        extensions = 'Buttons',
        option = list(
          autoWidth = TRUE,
          dom = 'Blfrtip',
          pageLength = -1,
          lengthMenu = list(c(10, 100, 1000,-1), c('10', '100', '1000', 'All')),
          buttons = c('copy', 'csv', 'excel')
        )
      )
    )
  
  #GGPLOTOS ABRA FUGGVENY
  plot_fuggveny <-
    function(plot_type = "base",
             kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt,
             x = merged_colnames[which(colnames(abra_df_react()) == input$tengelyvalaszt)],
             y = merged_colnames[which(colnames(abra_df_react()) == input$valuevalaszt)],
             fill = merged_colnames[which(colnames(abra_df_react()) == input$kategoriavalaszt)],
             area = "",
             label = "",
             subgroup = "") {
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      finished_plot <-
        ggplot(
          data = abra_df,
          aes(
            x =
              if (plot_type == "pie") {
                ""
              } else if (input$tengelyvalaszt == "DATUM") {
                as.Date(get(x))
              } else {
                get(x)
              },
            y = get(y),
            fill = get(fill)
          ),
          width = 1,
          stat = "identity"
        )
      
      return(finished_plot)
      
    }
  
  
  
  proba_plotly <-
    function(kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt,
             x = merged_colnames[which(colnames(abra_df_react()) == input$tengelyvalaszt)],
             y = merged_colnames[which(colnames(abra_df_react()) == input$valuevalaszt)],
             fill = merged_colnames[which(colnames(abra_df_react()) == input$kategoriavalaszt)])
    {
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      
      orig_color_list <-
        c(
          input$col_1,
          input$col_2,
          input$col_3,
          input$col_4,
          input$col_5,
          input$col_6,
          input$col_7,
          input$col_8,
          input$col_9,
          input$col_10,
          input$col_11,
          input$col_12
        )
      num_of_colors_orig <- length(orig_color_list)
      num_of_colors_needed <-
        abra_df %>% select(fill) %>% distinct() %>% unlist() %>% length()
      multiplier <-
        ceiling(num_of_colors_needed / num_of_colors_orig)
      final_color_list <- c()
      for (i in 1:multiplier) {
        final_color_list <- c(final_color_list, orig_color_list)
      }
      final_color_list <- final_color_list[1:num_of_colors_needed]
      
      finished_plot <-
        plotly::plot_ly(
          abra_df,
          x = ~
            if (input$tengelyvalaszt == "DATUM") {
              as.Date(get(x))
            } else {
              get(x)
            },
          y = ~ get(y),
          color = ~ get(fill),
          text = ~ get(y) / 10 ^ 9,
          textposition = 'inside',
          hoverinfo = "text",
          insidetextfont = list(size = 2, color = 'black'),
          colors = final_color_list,
          #fent van összerakva!!!
          type = 'bar'
        )
      
      
      
      return(finished_plot)
    }
  
  
  
  
  
  #PLOTLY ABRA FUGGVENY
  plot_fuggveny_plotly <-
    function(kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt) {
      abra_df <- abra_df_react()
      merged_colnames <- colnames(abra_df)
      
      
      finished_plot <-
        plotly::plot_ly(
          abra_df,
          labels = abra_df[, which(colnames(abra_df_react()) == input$kategoriavalaszt)],
          values = abra_df[, which(colnames(abra_df_react()) == input$valuevalaszt)],
          marker =
            list(
              colors = c(
                input$col_1,
                input$col_2,
                input$col_3,
                input$col_4,
                input$col_5,
                input$col_6,
                input$col_7,
                input$col_8,
                input$col_9,
                input$col_10
              )
            ),
          sort = T
        )
      
      return(finished_plot)
    }
  
  
  
  
  design <-  function(temp_plot) {
    layout(
      temp_plot,
      margin = list(t = 60, r = 50),
      
      plot_bgcolor = input$col_base,
      paper_bgcolor = input$col_base,
      
      xaxis =
        list(
          showgrid = TRUE,
          gridcolor = ifelse(input$xgrid == TRUE, input$xgrid_color, input$col_base),
          
          linecolor = input$xscale_color,
          linewidth = input$xscale_width,
          
          title =
            paste(
              "<br>",
              ifelse(
                length(input$x_font) == 0,
                input$x_text,
                case_when(
                  length(input$x_font) > 1 ~ paste("<b><i>", input$x_text, "<i></b>"),
                  input$x_font == "bold" ~ paste("<b>", input$x_text, "<b>"),
                  input$x_font == "italic" ~ paste("<i>", input$x_text, "<i>")
                )
              ),
              "<br> <br>"
            ),
          titlefont =
            list(
              family = input$font_type,
              size = input$x_size,
              color = input$x_color
            ),
          
          showticklabels = TRUE,
          tickcolor = ifelse(
            input$xscale_tick == TRUE,
            input$xscale_color,
            input$col_base
          ),
          tickangle = input$xscale_angle,
          tickfont =
            list(
              family = input$font_type,
              size = input$xscale_size,
              color = input$xscale_fontcolor
            )
        ),
      
      yaxis =
        list(
          showgrid = TRUE,
          gridcolor = ifelse(input$ygrid == TRUE, input$ygrid_color, input$col_base),
          
          linecolor = input$yscale_color,
          linewidth = input$yscale_width,
          
          title =
            paste(
              "<br>",
              ifelse(
                length(input$y_font) == 0,
                input$y_text,
                case_when(
                  length(input$y_font) > 1 ~ paste("<b><i>", input$y_text, "<i></b>"),
                  input$y_font == "bold" ~ paste("<b>", input$y_text, "<b>"),
                  input$y_font == "italic" ~ paste("<i>", input$y_text, "<i>")
                )
              ),
              "<br> <br>"
            ),
          titlefont =
            list(
              family = input$font_type,
              size = input$y_size,
              color = input$y_color
            ),
          
          showticklabels = TRUE,
          tickcolor = ifelse(
            input$yscale_tick == TRUE,
            input$yscale_color,
            input$col_base
          ),
          tickangle = input$yscale_angle,
          tickfont =
            list(
              family = input$font_type,
              size = input$yscale_size,
              color = input$yscale_fontcolor
            )
        ),
      
      
      legend =
        list(
          orientation = unlist(
            legend_orientation(input$legend_align, input$legend_adjustment)
          )[1],
          x = unlist(
            legend_orientation(input$legend_align, input$legend_adjustment)
          )[2],
          y = unlist(
            legend_orientation(input$legend_align, input$legend_adjustment)
          )[3],
          xanchor = "center",
          yanchor = "center",
          
          font =
            list(
              family = input$font_type,
              size = input$legend_textsize,
              color = input$legend_textcolor
            ),
          title =
            list(
              text = input$legend_titletext,
              font =
                list(
                  family = input$font_type,
                  size = input$legend_titlesize,
                  color = input$legend_titlecolor
                )
            )
        ),
      showlegend = ifelse(input$legend_align == "none", FALSE, TRUE),
      
      title =
        list(
          text = ifelse(
            length(input$title_font) == 0,
            input$title_text,
            case_when(
              length(input$title_font) > 1 ~ paste("<b><i>", input$title_text, "<i></b>"),
              input$title_font == "bold" ~ paste("<b>", input$title_text, "<b>"),
              input$title_font == "italic" ~ paste("<i>", input$title_text, "<i>")
            )
          ),
          font =
            list(
              family = input$font_type,
              size = input$title_size,
              color = input$title_color
            ),
          x = input$title_align,
          standoff = 20
        )
    )
    
  }
  
  
  
  
  #
  # list(
  #   ggtitle(input$title_text),
  #   xlab( input$x_text ),
  #   ylab( input$y_text ),
  #   labs( fill = input$legend_titletext ),
  
  # geom_text(
  #   label = "input$valuevalaszt",
  #   hjust = 0.5,
  #   position = position_dodge(width = 0.75),
  #   size = input$label_size,
  #   color = input$label_color,
  #   fontface = ifelse(length(input$label_font) > 1, "bold.italic", ifelse(length(input$label_font) == 0, "plain", input$label_font) ),
  #   check_overlap = TRUE
  # ),
  
  # theme(
  #panel.background = element_rect(fill = input$col_base),
  # plot.title = element_text(size = input$title_size,
  #                           family = input$font_type,
  #                           colour = input$title_color,
  #                           hjust = input$title_align,
  #                           face = ifelse(length(input$title_font) > 1, "bold.italic", ifelse(length(input$title_font) == 0, "plain", input$title_font) ) ),
  # axis.title.x = element_text(size = input$x_size,
  #                             vjust=1,
  #                             hjust=1,
  #                             family = input$font_type,
  #                             colour = input$x_color,
  #                             face = ifelse(length(input$x_font) > 1, "bold.italic", ifelse(length(input$x_font) == 0, "plain", input$x_font) ) ),
  # axis.title.y = element_text(size = input$y_size,
  #                             family = input$font_type,
  #                             colour = input$y_color,
  #                             face = ifelse(length(input$y_font) > 1, "bold.italic", ifelse(length(input$y_font) == 0, "plain", input$y_font) ) ),
  #
  # axis.text.x = element_text(size = input$xscale_size,
  #                            family = input$font_type,
  #                            colour = input$xscale_fontcolor,
  #                            angle = -input$xscale_angle),
  # axis.ticks.x = element_line(color = ifelse(input$xscale_tick == TRUE, input$xscale_color, input$col_base) ),
  # axis.line.x = element_line(color = input$xscale_color, size = input$xscale_width),
  #
  # axis.text.y = element_text(size = input$yscale_size,
  #                            family = input$font_type,
  #                            colour = input$yscale_fontcolor,
  #                            angle = -input$yscale_angle),
  # axis.ticks.y = element_line(color = ifelse(input$yscale_tick == TRUE, input$yscale_color, input$col_base) ),
  # axis.line.y = element_line(color = input$yscale_color, size = input$yscale_width),
  
  # legend.text = element_text(size = input$legend_textsize,
  #                            family = input$font_type,
  #                            colour = input$legend_textcolor),
  # legend.position = input$legend_align,
  # legend.title = element_text(size = input$legend_titlesize,
  #                             family = input$font_type,
  #                             colour = input$legend_titlecolor),
  
  # panel.grid.major = element_line(color = ifelse(input$majorgrid == TRUE, input$majorgrid_color,input$col_base) ),
  # panel.grid.minor = element_line(color = ifelse(input$minorgrid == TRUE, input$minorgrid_color,input$col_base) )
  # )
  
  # )
  
  
  
  
  
  
  
  
  
  
  legend_orientation <-
    function(input_orientation, input_legend_align) {
      position <-
        case_when(
          input_orientation == "right" ~ list('v', 1.1, 0.5),
          input_orientation == "bottom" ~ list('h', 0.5,-input_legend_align)
        )
      return(position)
    }
  
  
  
  
  p <-
    reactive({
      proba_plotly() %>% layout(barmode = 'stack') %>% design()
    }) #ORCAHOZ PROBA
  
  #bar output
  output$barplot <- plotly::renderPlotly({
    # proba_plotly() %>% layout(barmode = 'stack' ) %>% design()
    p()
    
    
    
  })
  
  #clustered
  output$barplotclust <- plotly::renderPlotly({
    plot_fuggveny() + geom_bar(position = "dodge", stat = "identity")
  })
  
  
  #savdiagram
  output$savplot <- plotly::renderPlotly({
    plot_fuggveny() + geom_col() + coord_flip()
    
  })
  
  #savdiagram clust
  output$savplotclust <- plotly::renderPlotly({
    plot_fuggveny() + geom_col(position = "dodge", stat = "identity") + coord_flip()
  })
  
  #point output
  output$pointplot <- plotly::renderPlotly({
    plot_fuggveny() + geom_point()
  })
  
  #area output
  output$areaplot <- plotly::renderPlotly({
    plot_fuggveny() + geom_area()
  })
  
  #line output
  output$lineplot <- plotly::renderPlotly({
    plot_fuggveny() + geom_line()
  })
  
  #pie output
  output$pieplot <- plotly::renderPlotly({
    plot_fuggveny_plotly() %>% plotly::add_pie() %>%
      design()
  })
  
  #donut output
  output$donutplot <- plotly::renderPlotly({
    plot_fuggveny_plotly() %>% plotly::add_pie(hole = 0.6) %>%
      
      layout(
        margin = list(t = 60, r = 50),
        legend =
          list(
            orientation = unlist(legend_orientation(input$legend_align))[1],
            x = unlist(legend_orientation(input$legend_align))[2],
            y = unlist(legend_orientation(input$legend_align))[3],
            font =
              list(
                family = input$font_type,
                size = input$legend_textsize + 3,
                color = input$legend_textcolor
              ),
            title =
              list(
                text = input$legend_titletext,
                font =
                  list(
                    family = input$font_type,
                    size = input$legend_titlesize + 5,
                    color = input$legend_titlecolor
                  )
              )
          ),
        showlegend = ifelse(input$legend_align == "none", FALSE, TRUE),
        title =
          list(
            text = ifelse(
              length(input$title_font) == 0,
              input$title_text,
              case_when(
                length(input$title_font) > 1 ~ paste("<b><i>", input$title_text, "<i></b>"),
                input$title_font == "bold" ~ paste("<b>", input$title_text, "<b>"),
                input$title_font == "italic" ~ paste("<i>", input$title_text, "<i>")
              )
            ),
            font =
              list(
                family = input$font_type,
                size = input$title_size + 3,
                color = input$title_color
              ),
            x = input$title_align,
            standoff = 20
          )
      )
    
  })
  
  
  
  #TREEMAP
  output$treemapplot <- renderPlot({
    ggplot(abra_df_react(),
           aes(
             area = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$valuevalaszt)]),
             fill = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$tengelyvalaszt)]),
             label = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$kategoriavalaszt)]),
             subgroup = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$tengelyvalaszt)])
           )) +
      geom_treemap() +
      geom_treemap_subgroup_border() +
      geom_treemap_subgroup_text(
        place = "centre",
        grow = T,
        alpha = 0.5,
        colour =
          "black",
        fontface = "italic",
        min.size = 0
      ) +
      geom_treemap_text(colour = "white",
                        place = "topleft",
                        reflow = T)
  })
  # Bookmarking code --------------------------
  # onBookmark(function(state) {
  #   state$values$szurt_db <- abra_df_react()
  #   state$values$szurt_kat <-res_mod()
  #
  # })
  #
  # onRestore(function(state) {
  #   vals$sum <- state$values$szurt_db
  # })
  #Bookmarking code --------------------------
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })
  
  # onBookmark(function(state) {
  #   alapk_valtozo <- input[["my-filters-ALAP_NEVE"]]
  #   state$values$alK <- alapk_valtozo
  # })
  
  
  
  onBookmark(function(state) {
    state$values$alapn <- input[["my-filters-ALAP_NEVE"]]
    state$values$alapkez <- input[["my-filters-ALAPKEZELO"]]
    state$values$befpol <-
      input[["my-filters-BEFPOL_SZERINTI_KATEGORIA"]]
    state$values$letetkez <- input[["my-filters-LETETKEZELO"]]
    state$values$alaptip <- input[["my-filters-ALAPTIPUS"]]
    state$values$alapfajta <- input[["my-filters-ALAPFAJTA"]]
    state$values$devizalis <-
      input[["my-filters-DEVIZALIS_KITETTSEG"]]
    state$values$foldrajzi <-
      input[["my-filters-FOLDRAJZI_KITETTSEG"]]
    state$values$egyeb <- input[["my-filters-EGYEB_KITETTSEG"]]
    state$values$devizanem <- input[["my-filters-DEVIZANEM"]]
    state$values$status <- input[["my-filters-STATUSZ"]]
    
    
  })
  
  onRestore(function(state) {
    vals_alapNev$sum <- state$values$alapn
    vals_alapkez$sum <- state$values$alapkez
    vals_befpol$sum <- state$values$befpol
    vals_letetkez$sum <- state$values$letetkez
    vals_alaptipus$sum <- state$values$alaptip
    vals_alapfajta$sum <- state$values$alapfajta
    vals_devizalis$sum <- state$values$devizalis
    vals_foldrajzi$sum <- state$values$foldrajzi
    vals_egyeb$sum <- state$values$egyeb
    vals_devizanem$sum <- state$values$devizanem
    vals_status$sum <- state$values$status
  })
  onRestored(function(state) {
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAPKEZELO",
      selected = "Aegon Magyarország Befektetési Alapkezelő Zrt.",
      choices = categories_df$ALAPKEZELO,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAP_NEVE",
      selected = vals_alapNev$sum,
      choices = categories_df$ALAP_NEVE,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-BEFPOL_SZERINTI_KATEGORIA",
      selected = vals_befpol$sum,
      choices = categories_df$BEFPOL_SZERINTI_KATEGORIA,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-LETETKEZELO",
      selected = vals_letetkez$sum,
      choices = categories_df$LETETKEZELO,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAPTIPUS",
      selected = vals_alaptipus$sum,
      choices = categories_df$ALAPTIPUS,
      server = TRUE
      
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAPFAJTA",
      selected = vals_alapfajta$sum,
      choices = categories_df$ALAPFAJTA,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-DEVIZALIS_KITETTSEG",
      selected = vals_devizalis$sum,
      choices = categories_df$DEVIZALIS_KITETTSEG,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-FOLDRAJZI_KITETTSEG",
      selected = vals_foldrajzi$sum,
      choices = categories_df$FOLDRAJZI_KITETTSEG,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-EGYEB_KITETTSEG",
      selected = vals_egyeb$sum,
      choices = categories_df$EGYEB_KITETTSEG,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-DEVIZANEM",
      selected = vals_devizanem$sum,
      choices = categories_df$DEVIZANEM,
      server = TRUE
      
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-STATUSZ",
      selected = vals_status$sum,
      choices = categories_df$STATUSZ,
      server = TRUE
      
    )
  })
  
  
  #selectize module bookmarkozásához
  # observeEvent(input$restore, {
  #
  #
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-ALAP_NEVE",
  #     selected = vals_alapNev$sum,
  #     choices=categories_df$ALAP_NEVE
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-ALAPKEZELO",
  #     selected = vals_alapkez$sum,
  #     choices=categories_df$ALAPKEZELO
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-BEFPOL_SZERINTI_KATEGORIA",
  #     selected = vals_befpol$sum,
  #     choices=categories_df$BEFPOL_SZERINTI_KATEGORIA
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-LETETKEZELO",
  #     selected = vals_letetkez$sum,
  #     choices=categories_df$LETETKEZELO
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-ALAPTIPUS",
  #     selected = vals_alaptipus$sum,
  #     choices=categories_df$ALAPTIPUS
  #
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-ALAPFAJTA",
  #     selected = vals_alapfajta$sum,
  #     choices=categories_df$ALAPFAJTA
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-DEVIZALIS_KITETTSEG",
  #     selected = vals_devizalis$sum,
  #     choices=categories_df$DEVIZALIS_KITETTSEG
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-FOLDRAJZI_KITETTSEG",
  #     selected = vals_foldrajzi$sum,
  #     choices=categories_df$FOLDRAJZI_KITETTSEG
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-EGYEB_KITETTSEG",
  #     selected = vals_egyeb$sum,
  #     choices=categories_df$EGYEB_KITETTSEG
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-DEVIZANEM",
  #     selected = vals_devizanem$sum,
  #     choices=categories_df$DEVIZANEM
  #
  #   )
  #   updateSelectizeInput(
  #     session,
  #     inputId = "my-filters-STATUSZ",
  #     selected = vals_status$sum,
  #     choices=categories_df$STATUSZ
  #
  #   )
  # })
  #
  
  
  #postolashoz
  # onRestore(function(state) {
  #   vals$sum <- state$values$szurt_db
  # })
  
  onBookmarked(
    fun = function(url) {
      POST(
        url = "https://vizutechadmin.hunelco.com/v1/power_bi/Qomj7lJpGfPOoIgu/reports",
        content_type_json(),
        body = paste0(
          '{\n  \"name\": \"',
          input$bookmarkcim,
          '\",\n  \"url\": \"',
          url,
          '\"\n}'
        )
      )
      x <- url
      #write.table(x,"srv/shiny-server/myapp/link.txt",sep="\t",row.names=FALSE)
    }
  )
  
  #csak, hogy lássam az urlt
  onBookmarked(
    fun = function(url) {
      output$text <- renderText({
        url
      })
    }
  )
  
  
  Sys.setenv("plotly_username" = "vizutech")
  Sys.setenv("plotly_api_key" = "F4sLsg5BoETo9uZ2UMHa")
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "shiny_plot.png"
    },
    content = function(file) {
      plotly_IMAGE(p(), out_file = file, scale = 10)
    }
  )
  
  
  #enableBookmarking(store = "server")
}

shinyApp(ui, server, enableBookmarking = "server")
