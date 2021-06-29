library(shiny)
library(bsearchtools)
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
#library(shinydashboardPlus)
library(treemapify)
library(dbplyr)
library(sqldf)
library(DT)
library(colourpicker)
library(rmarkdown)
library(htmlwidgets)
library(remotes) 
library(extrafont)
#install.packages("https://cran.r-project.org/src/contrib/Archive/shinydashboardPlus/shinydashboardPlus_0.7.5.tar.gz", repos=NULL, type="source")
font_import()


load("/srv/shiny-server/chartbuilder/.RData")
#timeseries_df <- mutate(head(timeseries_df,10000), DATUM = as.character(DATUM))


cleanMem <- function(n=10) { for (i in 1:n) gc() }

ui <-
  function(request) {
    dashboardPagePlus(
      #tags$head(tags$script(jscode)),
      #verbatimTextOutput("urlText"),
      collapse_sidebar = TRUE,
      header = dashboardHeaderPlus(
        fixed = FALSE,
        enable_rightsidebar = FALSE,
        rightSidebarIcon = "cog",
        left_menu =
          tagList(
            dropdownBlock(
              id = "save_dropdown",
              title = "Mentés",
              icon = icon("bold"),
              badgeStatus = NULL,
              
              column(
                width = 12,
                textInput(
                  inputId = "bookmarkcim",
                  label = "Mentett riport neve",
                  placeholder = "Tetszőleges elnevezés"
                ),
                bookmarkButton(id="bookmarkBtn",label="Mentés"),
                br()
              )
            )
            #actionButton("restore", "Mentett szűrők betöltése"),
            # ),
            # dropdownBlock(
            #   id = "download_dropdown",
            #   title = "Letöltés",
            #   icon = icon("download"),
            #   badgeStatus = NULL,
            #   
            #   column(
            #     width = 12,
            #     downloadButton(outputId = 'downloadPlot', label = 'Letöltés'),
            #     br()
            #   )
            #   
            # )
          )
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
        style = "background-color:#555a60;",
        setShadow(class = "dropdown-menu"),
        
        fluidRow(#tags$style(type='text/css', ".selectize-dropdown-content {max-height: 1000px !important; }"),
          column(
            4,
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
                    start = as.Date(max(dates_df$FORG_DATUM, na.rm=TRUE))-60,
                    end = max(dates_df$FORG_DATUM, na.rm=TRUE)
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
                  selectInput(
                    "tengelyvalaszt",
                    "Tengely:",
                    #merge(categories_df, timeseries_df %>% select(ISIN_KOD, DATUM)),
                    c(
                      "ISIN kód" = "ISIN_KOD",
                      "Alap neve" = "ALAP_NEVE",
                      "Alapkezelő" = "ALAPKEZELO",
                      "Alapkezelő rövidnév" = "ALAPKEZELO_ROVIDNEV",
                      "Letétkezelő" = "LETETKEZELO",
                      "Alaptípus" = "ALAPTIPUS",
                      "Alapfajta" = "ALAPFAJTA",
                      "Befektetési politika" = "BEFPOL_SZERINTI_KATEGORIA",
                      "Devizális kitettség" = "DEVIZALIS_KITETTSEG",
                      "Földrajzi kitettség" = "FOLDRAJZI_KITETTSEG",
                      "Egyéb kitettség" = "EGYEB_KITETTSEG",
                      "Devizanem" = "DEVIZANEM",
                      "Indulás dátuma" = "INDULAS_DATUMA",
                      "Státusz" = "STATUSZ",
                      "Dátum" = "DATUM"
                    ),
                    multiple = FALSE,
                    selected = "DATUM"
                  ),
                  
                  #switch: aggergalva legyen-e
                  conditionalPanel(
                    "input.tengelyvalaszt=='DATUM'",
                    # #datum havi heti stb. illetve datum aggregalva legyen-e?
                    materialSwitch(
                      inputId = "datumagr",
                      label = "Időben összevonva",
                      status = "default",
                      value =
                        FALSE
                    ),
                    conditionalPanel(
                      "input.datumagr == false",
                      selectInput(
                        "datumszuro",
                        "Dátumszűrő:",
                        c(
                          "Év utolsó napja" = "UNAP_EV",
                          "Negyedév utolsó napja" = "UNAP_NEGYEDEV",
                          "Hónap utolsó napja" = "UNAP_HONAP",
                          "Hét utolsó napja" = "UNAP_HET",
                          "Napi"
                        ),
                        selected =
                          "UNAP_HONAP"
                      ),
                    ),
                    conditionalPanel(
                      condition = "input.datumagr == true",
                      selectInput(
                        "osszevontdatumsz",
                        "Dátumszűrő",
                        c(
                          "Éves összevonás" = "AKT_EV",
                          "Negyedéves összevonás" =
                            "AKT_NEGYEDEV",
                          "Havi összevonás" =
                            "AKT_HONAP",
                          "Heti összevonás" =
                            "AKT_HET"
                        )
                      )
                    )
                  ),
                  
                  #kategoria, ertek kivalasztasa
                  selectInput(
                    "kategoriavalaszt",
                    "Jelmagyarázat:",
                    c(
                      "ISIN kód" = "ISIN_KOD",
                      "Alap neve" = "ALAP_NEVE",
                      "Alapkezelő" = "ALAPKEZELO",
                      "Alapkezelő (rövid név)" = "ALAPKEZELO_ROVIDNEV",
                      "Letétkezelő" = "LETETKEZELO",
                      "Alaptípus" = "ALAPTIPUS",
                      "Alapfajta" = "ALAPFAJTA",
                      "Befektetési politika" = "BEFPOL_SZERINTI_KATEGORIA",
                      "Devizális kitettség" = "DEVIZALIS_KITETTSEG",
                      "Földrajzi kitettség" = "FOLDRAJZI_KITETTSEG",
                      "Egyéb kitettség" = "EGYEB_KITETTSEG",
                      "Devizanem" = "DEVIZANEM",
                      "Indulás dátuma" = "INDULAS_DATUMA",
                      "Státusz" = "STATUSZ"
                    ),
                    multiple = FALSE,
                    selected = "ALAPFAJTA"
                  ),
                  
                  selectInput(
                    "valuevalaszt",
                    "Értékek:",
                    c(
                      "Dátum" = "DATUM",
                      "ISIN kód" = "ISIN_KOD",
                      "Nettó eszközérték (saját deviza)" = "NETTO_ESZKOZERTEK",
                      "Nettó eszközérték (HUF)" = "NETTO_ESZKOZERTEK_HUF",
                      "Árfolyam" = "ARFOLYAM",
                      "Befjegy forgalom (saját deviza)" = "NAPI_BEFJEGY_FORGALOM",
                      "Befjegy forgalom (HUF)" = "NAPI_BEFJEGY_FORGALOM_HUF",
                      "Befjegy forgalom (%)" = "NAPI_BEFJEGY_FORGALOM_SZAZALEK",
                      "Log hozam" = "LOG_HOZAM",
                      "Effektív hozam" = "EFF_HOZAM",
                      "Effektív hozam (3 hónapos)" = "EFF_HOZAM_3_HONAPOS",
                      "Effektív hozam (6 hónapos)" = "EFF_HOZAM_6_HONAPOS",
                      "Effektív hozam (1 éves)" = "EFF_HOZAM_1_EVES",
                      "Effektív hozam (3 éves)" = "EFF_HOZAM_3_EVES",
                      "Effektív hozam (5 éves)" = "EFF_HOZAM_5_EVES",
                      "Effektív hozam (10 éves)" = "EFF_HOZAM_10_EVES",
                      "Effektív hozam (év elejétől)" = "EFF_HOZAM_EV_ELEJETOL",
                      "Effektív hozam (indulástól)" = "EFF_HOZAM_INDULASTOL",
                      "Kifizetett hozamok" = "KIFIZETETT_HOZAMOK",
                      "Referencia index" = "REFERENCIAINDEX"
                    ),
                    multiple = FALSE,
                    selected = "NETTO_ESZKOZERTEK"
                  ),
                  
                  selectInput(
                    "osszegzofv",
                    "Összegző függvény:",
                    c(
                      "átlag" = "mean",
                      "összeg" = "sum",
                      "darab" = "count",
                      "max",
                      "min"
                    ),
                    selected =
                      "sum"
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
                      choices = extrafont::fonts()[112:length(extrafont::fonts())]
                    ),
                    colourpicker::colourInput("col_base", "Háttér szín", value = "white"),
                    selectInput(
                      "axis_order",
                      "Rendezés",
                      choices = c(
                        "betűrendben/időrendben" = 0, 
                        "érték szerint növekvő" = 1, 
                        "érték szerint csökkenő" = -1)
                    )
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
                        status = "warning"
                      ),
                      checkboxGroupButtons(
                        inputId = "title_font",
                        label = "Cím kiemelése:",
                        choiceNames = list(icon("bold"), icon("italic")),
                        choiceValues = list("bold", "italic"),
                        justified = TRUE,
                        status = "warning"
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
                      status = "warning"
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
                        status = "warning"
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
                        status = "warning"
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
                      numericInput("label_size", "Betűmérete:", "3"),
                      colourpicker::colourInput("label_color", "Színe", value = "black")
                    ),
                    column(
                      width = 6,
                      selectInput(
                        "label_format_unit",
                        "Egység:",
                        selected = 'F',
                        choices = c("(eredeti)", "ezer", "millió", "milliárd")
                      ),
                      numericInput(
                        "label_format_decimal",
                        "Tizedesjegyek:",
                        "0",
                        min = 0,
                        max = 3
                      )
                      #,
                      #checkboxInput("label_format_percent", "Mutatás százalékban (%)", value = F, width = NULL)
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
            div(boxPlus(
              width = 12,
              status = "danger",
              
              tabsetPanel(
                id = "chart_tabset",
                tabPanel('Táblázat', value =
                           "table", DT::DTOutput("tabletab")),
                tabPanel('Halmozott oszlop', value =
                           "stacked_bar", plotly::plotlyOutput('barplot')),
                tabPanel(
                  'Csoportosított oszlop',
                  value = "clustered_bar",
                  plotly::plotlyOutput('barplotclust')
                ),
                tabPanel(
                  'Halmozott sávdiagram',
                  value = "stacked_sav",
                  plotly::plotlyOutput('savplot')
                ),
                tabPanel(
                  'Csoportosított sávdiagram',
                  value = "clustered_sav",
                  plotly::plotlyOutput('savplotclust')
                ),
                tabPanel('Pontdiagram', value =
                           "scatter", plotly::plotlyOutput('pointplot')),
                tabPanel('Területdiagram', value =
                           "area", plotly::plotlyOutput('areaplot')),
                tabPanel('Vonaldiagram', value =
                           "line", plotly::plotlyOutput('lineplot')),
                tabPanel('Kördiagram', value =
                           "pie", plotly::plotlyOutput('pieplot')),
                tabPanel('Fánkdiagram', value =
                           "donut", plotly::plotlyOutput('donutplot'))
                #tabPanel('Treemap', plotOutput('treemapplot'))
                
              )
              
              
            )
            
            
            , style = "position:fixed; overflow-y:scroll; max-height:100%"),
            textOutput("text"),
            
            # mainPanel(
            #   h3("clientData values"),
            #   verbatimTextOutput("clientdataText"),
            # )
            
          ))
        
        
        
        ,
        
        tags$style(
          HTML(
            "

                                          .box.box-solid.box-warning>.box-header {
                                            color:#fff;
                                            background:#f2c080
                                          }

                                          .box.box-solid.box-warning {
                                              border-bottom-color:#000;
                                              border-left-color:#000;
                                              border-right-color:#000;
                                              border-top-color:#000;
                                          }

                                          .box.box-solid.box-warning>.box-header .box-title {
                                            color:#000
                                          }

                                          .skin-yellow .main-header .navbar {
                                              background-color: #f2c080;
                                          }

                                          .skin-yellow .main-header .navbar .nav>li>a {
                                              color: #000;
                                          }

                                          # .skin-yellow .main-header .navbar .sidebar-toggle:hover {
                                          #     background-color: #deb887;
                                          # }

                                          .skin-yellow .main-header .logo {
                                              background-color: #f2c080;
                                          }

                                          .content-wrapper {
                                              background-color: #555a60;
                                          }

                                          h2 {
                                              color: #fff;
                                          }


                                        "
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
  
  
  #szurok alkalmazasa categories tablan  
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
    bsearchtools::DFI.subset(timeseries_dfi,
                             AND(RG("DATUM",
                                    input$dateRange[1], input$dateRange[2]), 
                                 IN("ISIN_KOD", as.character(res_mod()$ISIN_KOD)))) %>%
      select(ISIN_KOD,!!!input$valuevalaszt, DATUM)
    
  })
  
  
  # leszurt ISINek kivalasztasa a datumra leszurt timeseries tablabol
  # react_isin_szurt <- reactive({
  #   react_ts() %>% filter(ISIN_KOD %in% res_mod()$ISIN_KOD)
  # })
  
  #ISIN kod, kivalasztott timeseries ertek kivalasztasa egy tablaba,
  #majd ezt fogom mergelni a categories_df-hez
  # react_selected <- reactive({
  #   react_isin_szurt() %>% select(ISIN_KOD,!!!input$valuevalaszt, DATUM)
  # })
  
  #leszurom a categories tablat
  # react_categories_szurt_ISIN <- reactive({
  #   categories_df %>% 
  #     filter(ISIN_KOD %in% res_mod()$ISIN_KOD)
  # })
  # 
  #########################kerdes: csinalhatnam azt hogy az elejen mergelem az egeszet, igy utolag mar nem kell uj mergelest csinalni csak selectet
  
  #merge abra_df (mergelem a kategoria szukseges oszlopat a timeseries oszlopokkal),
  #ha datum a tengely, akkor itt ne vÃ¡lassza ki mÃ©gegyszer, mert akkor egy uresre kicserelne
  #a mar kivalasztott datum oszlopot, ezert van az ifelse elagazas
  abra_df_react_dates_df_nelkul <- reactive({
    merge(
      if (input$tengelyvalaszt == "DATUM") {
        res_mod() %>% select(ISIN_KOD,!!!input$kategoriavalaszt)
      } else {
        res_mod() %>% select(ISIN_KOD,
                             !!!input$kategoriavalaszt,
                             !!!input$tengelyvalaszt)
      },
      react_ts(),
      by.x = "ISIN_KOD",
      by.y = "ISIN_KOD"
    ) %>% select(!ISIN_KOD)
  })
  
  
  
  #  #nem Ã¶sszevonos eseten: hozzaadom a datum oszlopot (ha napit ad hozzÃ¡, akor csak siman a datumot adom hozza, kuonben meg azt az oszlopot amit kivalaszt: utolso het, utolso honap stb)
  #osszevonos: kivÃ¡lasztja currentyear, currentweek megfelelo oszlopot
  abra_df_react_non_grouped <- reactive({
    if (input$datumagr == FALSE)  {
      #ha nem osszevonos
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
    } else {
      #ha osszevonos
      merge(abra_df_react_dates_df_nelkul(),
            dates_df %>%
              select(DATUM, which(
                colnames(dates_df) == input$osszevontdatumsz
              )))
    }
  })
  
  
  
  
  #fuggveny, ami csinal group by tablat, amit majd Ã¶sszegzo fuggveny input alapjan hasznalunk
  group_data <- function(df, y, x, fill, fv, d_osszevont) {
    fv <- get(fv)
    if (input$datumagr == FALSE) {
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
    } else {
      df2 <- as.data.frame(
        df %>% select(y, d_osszevont, fill) %>%
          group_by(get(d_osszevont), get(fill)) %>%
          dplyr::summarise(sumArfolyam = fv(get(y)))
      )
      df2 <- as.data.frame(df2)
      
      colnames(df2) <- c(d_osszevont, fill, y)
      
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
        fv = input$osszegzofv,
        d_osszevont = as.character(input$osszevontdatumsz)
      )
    })
  
  
  #  #nem Ã¶sszevonos eseten: hozzaadom a datum oszlopot (ha napit ad hozzÃ¡, akor csak siman a datumot adom hozza, kuonben meg azt az oszlopot amit kivalaszt: utolso het, utolso honap stb)
  #osszevonos: kivÃ¡lasztja currentyear, currentweek megfelelo oszlopot
  abra_df_react_non_grouped <- reactive({
    if (input$datumagr == FALSE)  {
      #ha nem osszevonos
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
    } else {
      #ha osszevonos
      merge(abra_df_react_dates_df_nelkul(),
            dates_df %>%
              select(DATUM, which(
                colnames(dates_df) == input$osszevontdatumsz
              )))
    }
  })
  
  
  
  
  
  #fuggveny, ami csinal group by tablat, amit majd Ã¶sszegzo fuggveny input alapjan hasznalunk
  group_data <- function(df, y, x, fill, fv, d_osszevont) {
    fv <- get(fv)
    if (input$datumagr == FALSE) {
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
    } else {
      df2 <- as.data.frame(
        df %>% select(y, d_osszevont, fill) %>%
          group_by(get(d_osszevont), get(fill)) %>%
          dplyr::summarise(sumArfolyam = fv(get(y)))
      )
      df2 <- as.data.frame(df2)
      
      colnames(df2) <- c(d_osszevont, fill, y)
      
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
        fv = input$osszegzofv,
        d_osszevont = as.character(input$osszevontdatumsz)
      )
    })
  
  #mergelem a lekerdezett reactive time-series df valtozot a categories-zal
  output$tabletab <-
    #DT::renderDT(abra_df_react())
    DT::renderDT(
      DT::datatable(
        #abra_df_react(),
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
             x = merged_colnames[which(
               colnames(abra_df_react()) == ifelse(
                 input$datumagr == FALSE,
                 input$tengelyvalaszt,
                 input$osszevontdatumsz
               )
             )],
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
  
  
  # chartba megcsinalja az aggregationt ahhoz, hogy orderelni lehessen normálisan
  # MÉGSE KELL OMG!!!
  # aggregate_values_for_order <- function(x_var, y_var) {
  #   temp_df <- data.frame(x = x_var, y = y_var) %>% group_by(x)
  #   agg_temp_df <- temp_df %>% summarise(agg_val = sum(y))
  #   temp_df <- merge(temp_df$x, agg_temp_df, by="x", sort = F)
  #   final_list <- temp_df$agg_val %>% unlist()
  #   return(final_list)
  # }
  
  # chartba megcsinalja a number formattingot
  number_formatting <-
    function(szam, egyseg, tizedesjegy, szazalek) {
      #egyseg
      if (egyseg == "ezer") {
        szam <- szam / 10 ^ 3
      } else if (egyseg == "millió") {
        szam <- szam / 10 ^ 6
      } else if (egyseg == "milliárd") {
        szam <- szam / 10 ^ 9
      } else {
        szam <- szam
      }
      
      #szazalek
      if (szazalek == TRUE) {
        szam <- szam / 100
      }
      
      #tizedesjegy
      szam <-
        format(round(szam, digits = tizedesjegy), nsmall = tizedesjegy)
      
      return(szam)
      
    }
  
  
  proba_plotly <-
    function(chart_type,
             mode = "none",
             kivalasztott_ertek = input$valuevalaszt,
             kategoria = input$kategoriavalaszt,
             x = merged_colnames[which(
               colnames(abra_df_react()) == ifelse(
                 input$datumagr == FALSE,
                 input$tengelyvalaszt,
                 input$osszevontdatumsz
               )
             )],
             y = merged_colnames[which(colnames(abra_df_react()) == input$valuevalaszt)],
             fill = merged_colnames[which(colnames(abra_df_react()) == input$kategoriavalaszt)])
    {
      abra_df <- abra_df_react()
      abra_df[3] <-
        lapply(
          abra_df[3],
          number_formatting,
          egyseg = input$label_format_unit,
          tizedesjegy = input$label_format_decimal,
          szazalek = F
        )#input$label_format_percent) #fent van összerakva!!!
      abra_df[3] <- lapply(abra_df[3], as.numeric)
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
      
      
      if (mode == "marker") {
        mode_val <- "markers"
        fill_val <- "none"
      } else if (mode == "line") {
        mode_val <- "lines+markers"
        fill_val <- "none"
      } else if (mode == "area") {
        mode_val <- "none"
        fill_val <- "tonexty"
      }
      
      
      
      finished_plot <-
        if (chart_type == "bar") {
          plotly::plot_ly(
            abra_df,
            x = ~
              if (input$tengelyvalaszt == "DATUM") {
                get(x)
              } else {
                
                reorder (factor(get(x), levels = unique(c(
                  as.character(get(x))
                ))) , as.numeric(input$axis_order) * get(y), FUN = sum)
                
              },
            y = ~ get(y),
            color = ~ get(fill),
            text = ~ get(y),
            hoverinfo = "text",
            textposition = 'inside',
            insidetextfont = list(color = input$label_color),
            outsidetextfont = list(color = input$label_color),
            insidetextanchor = "middle",
            textangle = 0,
            colors = final_color_list,
            #fent van összerakva!!!
            type = 'bar',
            orientation = 'v'
          ) %>% 
            plotly::config(
              toImageButtonOptions = list(format = "png", scale=20),
              displaylogo = FALSE,
              modeBarButtonsToRemove = list(
                'sendDataToCloud',
                'lasso2d',
                'hoverClosestCartesian',
                'hoverCompareCartesian',
                'toggleSpikelines'
              )
            )
          
        } else if (chart_type == 'sav') {
          plotly::plot_ly(
            abra_df,
            x = ~ get(y),
            y = ~
              if (input$tengelyvalaszt == "DATUM") {
                get(x)
              } else {
                
                reorder (factor(get(x), levels = unique(c(
                  as.character(get(x))
                ))) , as.numeric(input$axis_order) * get(y), FUN = sum)
                
              },
            color = ~ get(fill),
            text = ~ get(y),
            hoverinfo = "text",
            textposition = 'inside',
            insidetextfont = list(color = input$label_color),
            outsidetextfont = list(color = input$label_color),
            insidetextanchor = "middle",
            textangle = 0,
            colors = final_color_list,
            #fent van összerakva!!!
            type = 'bar',
            orientation = 'v'
          ) %>% 
            plotly::config(
              toImageButtonOptions = list(format = "png", scale=20),
              displaylogo = FALSE,
              modeBarButtonsToRemove = list(
                'sendDataToCloud',
                'lasso2d',
                'hoverClosestCartesian',
                'hoverCompareCartesian',
                'toggleSpikelines'
              )
            )
          
        } else if (chart_type == "pie") {
          plotly::plot_ly(
            abra_df,
            labels = ~ get(fill),
            values = ~ get(y),
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
                  input$col_10,
                  input$col_11,
                  input$col_12
                )
              ),
            sort = ifelse(input$axis_order == 0, FALSE, TRUE),
            textfont = list(
              color = input$label_color,
              size = input$label_size,
              family = input$font_type
            ),
            #texttemplate = "%{label}: %{value} <br>(%{percent})"
            texttemplate = "%{value} <br>(%{percent})"
          ) %>% 
            plotly::config(
              toImageButtonOptions = list(format = "png", scale=20),
              displaylogo = FALSE,
              modeBarButtonsToRemove = list(
                'sendDataToCloud',
                'lasso2d',
                'hoverClosestCartesian',
                'hoverCompareCartesian',
                'toggleSpikelines'
              )
            )
          
        } else if (chart_type == "scatter") {
          plotly::plot_ly(
            abra_df,
            x = ~ as.Date(get(x)),
            y = ~ get(y),
            color = ~ get(fill),
            text = ~ get(y),
            hoverinfo = "text",
            textposition = 'inside',
            colors = final_color_list,
            #fent van összerakva!!!
            type = 'scatter',
            mode = mode_val,
            fill = fill_val,
            #fillcolor = final_color_list, # EZ NEM TOM MIERT NEM MEGY IGY
            fillcolor = list(
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
                input$col_10,
                input$col_11,
                input$col_12
              )
            ),
            stackgroup = 'one',
            connectgaps = TRUE,
            opacity = 1
            #groupnorm = 'percent'
          ) %>% 
            plotly::config(
              toImageButtonOptions = list(format = "png", scale=20),
              displaylogo = FALSE,
              modeBarButtonsToRemove = list(
                'sendDataToCloud',
                'lasso2d',
                'hoverClosestCartesian',
                'hoverCompareCartesian',
                'toggleSpikelines'
              )
            )
          
        }
      
      
      
      return(finished_plot)
    }
  
  
  
  
  design <-  function(temp_plot) {
    layout(
      temp_plot,
      
      separators = ",.",
      
      uniformtext =
        list(mode = "show",
             minsize = input$label_size),
      
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
  
 
  legend_orientation <-
    function(input_orientation, input_legend_align) {
      position <-
        case_when(
          input_orientation == "right" ~ list('v', 1.1, 0.5),
          input_orientation == "bottom" ~ list('h', 0.5,-input_legend_align)
        )
      return(position)
    }
  
  
  #final chart: visszaadja az elkészült plotlyt bekéri, hogy milyen fajta chartot akarunk, es azt keri vissza
  #
  final_chart <- function(chart_type, mode)
  {
    if (chart_type == "bar" & mode == "stack") {
      proba_plotly(chart_type = "bar") %>% layout(barmode = 'stack') %>% design()
    } else if (chart_type == "bar" & mode == "group") {
      proba_plotly(chart_type = "bar") %>% layout(barmode = 'group') %>% design()
    } else if (chart_type == "sav" & mode == "stack") {
      proba_plotly(chart_type = "sav") %>% layout(barmode = 'stack') %>% design()
    } else if (chart_type == "sav" & mode == "group") {
      proba_plotly(chart_type = "sav") %>% layout(barmode = 'group') %>% design()
    } else if (chart_type == "pie" & mode == "base") {
      proba_plotly(chart_type = "pie") %>% plotly::add_pie() %>% design()
    } else if (chart_type == "pie" & mode == "donut") {
      proba_plotly(chart_type = "pie") %>% plotly::add_pie(hole = 0.6) %>% design()
    } else if (chart_type == "scatter" & mode == "area") {
      proba_plotly(chart_type = "scatter", mode = "area") %>% design()
    } else if (chart_type == "scatter" & mode == "marker") {
      proba_plotly(chart_type = "scatter", mode = "marker") %>% design()
    } else if (chart_type == "scatter" & mode == "line") {
      proba_plotly(chart_type = "scatter", mode = "line") %>% design()
    }
  }
  #megnezi, h melyik tabset az aktiv es annak megfeleloen megmondja a downloadhandlernek kesobb, h 
  #melyiket toltse le, kijeloli gyakorlatilag, h milyen inputot kell megetetni final_charttal
  active_chart <- function() {
    if (input$chart_tabset == "stacked_bar") {
      active_plot <- final_chart(chart_type = "bar", mode = "stack")
    } else if (input$chart_tabset == "clustered_bar") {
      active_plot <- final_chart(chart_type = "bar", mode = "group")
    } else if (input$chart_tabset == "stacked_sav") {
      active_plot <- final_chart(chart_type = "sav", mode = "stack")
    } else if (input$chart_tabset == "clustered_sav") {
      active_plot <- final_chart(chart_type = "sav", mode = "group")
    } else if (input$chart_tabset == "scatter") {
      active_plot <- final_chart(chart_type = "scatter", mode = "marker")
    } else if (input$chart_tabset == "area") {
      active_plot <- final_chart(chart_type = "scatter", mode = "area")
    } else if (input$chart_tabset == "line") {
      active_plot <- final_chart(chart_type = "scatter", mode = "line")
    } else if (input$chart_tabset == "pie") {
      active_plot <- final_chart(chart_type = "pie", mode = "base")
    } else if (input$chart_tabset == "donut") {
      active_plot <- final_chart(chart_type = "pie", mode = "donut")
    } else {
      active_plot <-
        final_chart(chart_type = "bar", mode = "stack") #csak hogy legyen valami
    }
    
    return(active_plot)
  }
 
  rendered_active_chart <- plotly::renderPlotly({ active_chart() })
  
  output$barplot <- rendered_active_chart
  output$barplotclust <- rendered_active_chart
  output$savplot <- rendered_active_chart
  output$savplotclust <- rendered_active_chart
  output$pointplot <- rendered_active_chart
  output$areaplot <- rendered_active_chart
  output$lineplot <- rendered_active_chart
  output$pieplot <- rendered_active_chart
  output$donutplot <- rendered_active_chart

  

  #TREEMAP
  output$treemapplot <- renderPlot({
    ggplot(abra_df_react(),
           aes(
             area = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$valuevalaszt)]),
             fill = get(colnames(abra_df_react())[which(
               colnames(abra_df_react()) == ifelse(
                 input$datumagr == FALSE,
                 input$tengelyvalaszt,
                 input$osszevontdatumsz
               )
             )]),
             label = get(colnames(abra_df_react())[which(colnames(abra_df_react()) == input$kategoriavalaszt)]),
             subgroup = get(colnames(abra_df_react())[which(
               colnames(abra_df_react()) == ifelse(
                 input$datumagr == FALSE,
                 input$tengelyvalaszt,
                 input$osszevontdatumsz
               )
             )])
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
  setBookmarkExclude("bookmarkBtn")
  # 
  # onBookmark(function(state) {
  #   alapk_valtozo <- input[["my-filters-ALAP_NEVE"]]
  #   state$values$alK <- alapk_valtozo
  # })
  
  # updateSelectizeInput(
  #   session,
  #   inputId = "my-filters-ALAPKEZELO",
  #   choices=categories_df$ALAPKEZELO,
  #   server=TRUE
  # )
  
  
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
  
  
  # onRestore(function(state) {
  #   vals_alapNev$sum <- state$values$alapn
  #   vals_alapkez$sum <- state$values$alapkez
  #   vals_befpol$sum <- state$values$befpol
  #   vals_letetkez$sum <- state$values$letetkez
  #   vals_alaptipus$sum <- state$values$alaptip
  #   vals_alapfajta$sum <- state$values$alapfajta
  #   vals_devizalis$sum <- state$values$devizalis
  #   vals_foldrajzi$sum <- state$values$foldrajzi
  #   vals_egyeb$sum <- state$values$egyeb
  #   vals_devizanem$sum <- state$values$devizanem
  #   vals_status$sum <- state$values$status
  # })
  onRestore(function(state) {
    vals$sum <- state$values$szurt_db
    vals_alapkez$sum <- state$values$alapkez
    vals_alapNev$sum <- state$values$alapn
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
      selected = vals_alapkez$sum,
      choices = categories_df$ALAPKEZELO
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAP_NEVE",
      selected = vals_alapNev$sum,
      choices = categories_df$ALAP_NEVE
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-BEFPOL_SZERINTI_KATEGORIA",
      selected = vals_befpol$sum,
      choices = categories_df$BEFPOL_SZERINTI_KATEGORIA
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-LETETKEZELO",
      selected = vals_letetkez$sum,
      choices = categories_df$LETETKEZELO
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAPTIPUS",
      selected = vals_alaptipus$sum,
      choices = categories_df$ALAPTIPUS
      
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-ALAPFAJTA",
      selected = vals_alapfajta$sum,
      choices = categories_df$ALAPFAJTA
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-DEVIZALIS_KITETTSEG",
      selected = vals_devizalis$sum,
      choices = categories_df$DEVIZALIS_KITETTSEG
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-FOLDRAJZI_KITETTSEG",
      selected = vals_foldrajzi$sum,
      choices = categories_df$FOLDRAJZI_KITETTSEG
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-EGYEB_KITETTSEG",
      selected = vals_egyeb$sum,
      choices = categories_df$EGYEB_KITETTSEG
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-DEVIZANEM",
      selected = vals_devizanem$sum,
      choices = categories_df$DEVIZANEM
    )
    updateSelectizeInput(
      session,
      inputId = "my-filters-STATUSZ",
      selected = vals_status$sum,
      choices = categories_df$STATUSZ
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
  
  atiranyitsak <-
    reactive({
      stringr::str_detect(session$clientData$url_search, "companyId")
    })
  values <- reactiveValues(link_react="http://befalapok.vizu-tech.com/chartbuilder/?_state_id_=3a768951b3451c2d&companyId=Qomj7lJpGfPOoIgu&uid=qomj7lJpGfPOoIgu&categoryId=pDy98.W1BBdYbcl~")
  observeEvent(atiranyitsak()==TRUE, {
    values$link_react <- paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      session$clientData$url_pathname,
      session$clientData$url_search)
    
  })
  
  
  onBookmarked(
    fun = function(url) {
      POST(
        url = paste0("https://vizutechadmin.hunelco.com/v1/power_bi/", url_splitter(values$link_react)[1], "/reports"),
        content_type("multipart/form-data"),
        body = list(data=paste0(
          '{\n  \"name\": \"', as.character(input$bookmarkcim), 
          '\",\n  \"url\": \"', url, 
          '\",\n  \"categoryId\": \"', url_splitter(values$link_react)[3], 
          '\",\n  \"type\": \"', "shiny", '\"\n}')), encode="multipart"
      )
      # x <- url
      #write.table(x,"srv/shiny-server/myapp/link.txt",sep="\t",row.names=FALSE)
    }
  )
  
  #csak, hogy lássam az urlt
  #onBookmarked( fun = function(url) { output$text <- renderText({	 class(timeseries_df$DATUM)    }) } )
  
  
  Sys.setenv("plotly_username" = "vizutech")
  Sys.setenv("plotly_api_key" = "F4sLsg5BoETo9uZ2UMHa")
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      "shiny_plot.png"
    },
    content = function(file) {
      plotly_IMAGE(active_chart(), out_file = file, scale = 5)
    }, contentType="image/png"
  )
  
  
  
  
  # clientdata mentese
  cdata <- session$clientData
  
  
  
  
  # url_kiszedett <- reactive({
  #   paste0(
  #     session$clientData$url_hostname,
  #     session$clientData$url_pathname,
  #     session$clientData$url_search
  #   )
  # })
  url_splitter <- function(url) {
    proba_link_splited <-  stringr::str_split(url, "\\&")
    link_state_category <- proba_link_splited[[1]][1]
    link_and_state_id <- paste(unlist(stringr::str_split(as.character(link_state_category), "\\?"))[[1]][1],
                               unlist(stringr::str_split(as.character(link_state_category), "\\?"))[2], sep="?")
    link_no_state_id <-
      unlist(stringr::str_split(as.character(link_state_category), "\\?"))[1]
    
    companyID <- ifelse(stringr::str_detect(url, "state_id") == TRUE,
                        proba_link_splited[[1]][2],
                        unlist(stringr::str_split(as.character(proba_link_splited[[1]][1]), "\\?"))[2])
    
    companyID <- (stringr::str_split(companyID, "\\=") %>% unlist)[2]
    
    uid <-
      ifelse(
        stringr::str_detect(url, "state_id") == TRUE,
        proba_link_splited[[1]][3],
        proba_link_splited[[1]][2]
      )
    uid <- (stringr::str_split(uid, "\\=") %>% unlist)[2]
    category_id <-
      ifelse(
        stringr::str_detect(url, "state_id") == TRUE,
        proba_link_splited[[1]][4],
        proba_link_splited[[1]][3]
      )
    category_id <- (stringr::str_split(category_id, "\\=") %>% unlist)[2]
    vegso_link <-
      ifelse(
        stringr::str_detect(url, "state_id") == TRUE,
        link_and_state_id,
        link_no_state_id
      )
    return(c(companyID, uid, category_id, vegso_link))
  }
  
  
  
  observeEvent(atiranyitsak(), {
    if (atiranyitsak() == TRUE) {
      session$sendCustomMessage("mymessage",
                                url_splitter(
                                  paste0(
                                    session$clientData$url_protocol,
                                    "//",
                                    session$clientData$url_hostname,
                                    session$clientData$url_pathname,
                                    session$clientData$url_search
                                    
                                  )
                                )[4])
    }
  })
  
  
  
  
}

shinyApp(ui, server, enableBookmarking = "server")
