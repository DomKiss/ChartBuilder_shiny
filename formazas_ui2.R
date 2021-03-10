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
library(shinyscreenshot)


ui <- 
  function(request){dashboardPagePlus( collapse_sidebar = TRUE,
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
                                       
                                       sidebar = dashboardSidebar( 
                                         collapsed=TRUE,
                                         
                                         sidebarMenu(
                                           menuItem("Szűrők", tabName = "szurok", icon=icon("filter") ),
                                           menuItem("Diagram-elemek", tabName = "diagram", icon=icon("area-chart") ),
                                           menuItem("Színezés", tabName = "szinezes", icon=icon("paint-brush") )
                                           
                                         )
                                         
                                       ),
                                       
                                       body = dashboardBody( style = "background-color:#444444;",
                                                             setShadow(class = "dropdown-menu"),
                                                             
                                                             fluidRow(
                                                               #tags$style(type='text/css', ".selectize-dropdown-content {max-height: 1000px !important; }"),
                                                               column(4,
                                                                      bookmarkButton(id = "bookmarkBtn"),
                                                                      textInput(inputId = "bookmarkcim", label="Mentett riport neve", placeholder = "Data Summary"),
                                                                      actionButton("restore", "Mentett szűrők betöltése"),
                                                                      tabItems(
                                                                        tabItem(tabName = "szurok", h2("Szűrők"),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Dátumszűrő", status="warning", solidHeader = T,
                                                                                         dateRangeInput('dateRange',
                                                                                                        label = 'Dátum kivalasztasa',
                                                                                                        start = "2020-08-03", end = "2020-08-05"
                                                                                         )     
                                                                                ),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Tengely- és értékszűrők", status="warning", solidHeader = T,
                                                                                         #kategoria, ertek kivalasztasa
                                                                                         varSelectInput(
                                                                                           "tengelyvalaszt",
                                                                                           "Tengely:",
                                                                                           merge(categories_df, timeseries_df %>% select(ISIN_KOD, DATUM)),
                                                                                           multiple = FALSE,
                                                                                           selected = "ALAPKEZELO_ROVIDNEV"
                                                                                         ),
                                                                                         
                                                                                         #datum havi heti stb. illetve datum aggregalva legyen-e?
                                                                                         conditionalPanel("input.tengelyvalaszt=='DATUM'", 
                                                                                                          selectInput("datumszuro", "datumszuro", c("UNAP_EV", "UNAP_NEGYEDEV", "UNAP_HONAP", "UNAP_HET", "Napi"), selected="Napi"),
                                                                                         ),
                                                                                         
                                                                                         #switch: aggergalva legyen-e
                                                                                         conditionalPanel("input.tengelyvalaszt=='DATUM'", materialSwitch(inputId = "datumagr", label = "Időben összevonva", status = "primary")),
                                                                                         
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
                                                                                         
                                                                                         selectInput("osszegzofv", "Összegző függvény", c("mean", "sum", "count", "max", "min"), selected="sum")
                                                                                         
                                                                                         
                                                                                ),  
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Kategóriaszűrők", status="warning", solidHeader = T,
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
                                                                                               options=list(maxOptions=20000)
                                                                                             )
                                                                                           ),
                                                                                           status = "warning"
                                                                                         )         
                                                                                         
                                                                                )
                                                                                
                                                                        ),
                                                                        
                                                                        
                                                                        tabItem(tabName = "diagram", h2("Diagram-elemek"),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Vászon", status="warning", solidHeader = T,
                                                                                         fluidRow(
                                                                                           column(
                                                                                             width = 12,
                                                                                             selectInput(
                                                                                               "font_type",
                                                                                               "Betűtípus",
                                                                                               selected = 'F',
                                                                                               choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
                                                                                             ),
                                                                                             colourpicker::colourInput("col_base", "Háttér szín", value = "white")
                                                                                           )
                                                                                         ),
                                                                                         fluidRow(
                                                                                           column(
                                                                                             width=6,
                                                                                             checkboxInput("xgrid", "X tengely rácsvonalak", value = F, width = NULL),
                                                                                             conditionalPanel(
                                                                                               condition = "input.xgrid > 0",
                                                                                               colourpicker::colourInput("xgrid_color", "Rácsvonal színe", value = "white")
                                                                                             ) 
                                                                                           ),
                                                                                           column(
                                                                                             width=6,
                                                                                             checkboxInput("ygrid", "Y tengely rácsvonalak", value = F, width = NULL),
                                                                                             conditionalPanel(
                                                                                               condition = "input.ygrid > 0",
                                                                                               colourpicker::colourInput("ygrid_color", "Rácsvonal színe", value = "white")
                                                                                             )
                                                                                           )
                                                                                         )
                                                                                ),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Cím", status="warning", solidHeader = T,
                                                                                         fluidRow(
                                                                                           column(
                                                                                             width = 12,
                                                                                             textInput('title_text', 'Cím megadása:', value = "Default cím")
                                                                                           )
                                                                                         ),
                                                                                         fluidRow(
                                                                                           column(
                                                                                             width=6,
                                                                                             radioGroupButtons(
                                                                                               inputId = "title_align",
                                                                                               label = "Cím igazítása:",
                                                                                               choiceNames = list( icon("align-left"), icon("align-center"), icon("align-right") ),
                                                                                               choiceValues = list(0, 0.5, 1),
                                                                                               justified = TRUE,
                                                                                               status = "primary"
                                                                                             ),
                                                                                             checkboxGroupButtons(
                                                                                               inputId = "title_font",
                                                                                               label = "Cím kiemelése:",
                                                                                               choiceNames = list( icon("bold"), icon("italic")),
                                                                                               choiceValues = list("bold", "italic"),
                                                                                               justified = TRUE,
                                                                                               status = "primary"
                                                                                             )
                                                                                           ),
                                                                                           column(
                                                                                             width=6,
                                                                                             numericInput("title_size", "Cím betűmérete:", "22"),
                                                                                             colourpicker::colourInput("title_color", "Cím színe:", value = "black")
                                                                                           )
                                                                                         )
                                                                                ),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Jelmagyarázat", status="warning", solidHeader = T,
                                                                                         fluidRow( 
                                                                                           column( 
                                                                                             width=12, 
                                                                                             radioGroupButtons(
                                                                                               inputId = "legend_align",
                                                                                               label = "Jelmagyarázat helye:",
                                                                                               choiceNames = list( 
                                                                                                 icon("ban"), 
                                                                                                 icon("arrow-right"), 
                                                                                                 icon("arrow-down") 
                                                                                               ),
                                                                                               choiceValues = list("none", "right", "bottom"),
                                                                                               justified = TRUE,
                                                                                               status = "primary"
                                                                                             )
                                                                                           )
                                                                                         ),
                                                                                         fluidRow(
                                                                                           conditionalPanel(
                                                                                             condition = "input.legend_align !== 'none'",
                                                                                             column(width=6,
                                                                                                    h4("Jelm. címe"),
                                                                                                    textInput('legend_titletext', 'Cím megadása:', value = "Jelmagyarázat"),
                                                                                                    numericInput("legend_titlesize", "Betűmérete:", "12"),
                                                                                                    colourpicker::colourInput("legend_titlecolor", "Betűszíne:", value = "black")
                                                                                             ),
                                                                                             column(width=6,
                                                                                                    h4("Jelm. szövege"),
                                                                                                    #numericInput("legend_markersize", "Marker mérete:", "2"),
                                                                                                    numericInput("legend_textsize", "Betűmérete:", "9"),
                                                                                                    colourpicker::colourInput("legend_textcolor", "Betűszíne:", value = "black")
                                                                                             )
                                                                                           ),
                                                                                           column(width=12,
                                                                                                  conditionalPanel( 
                                                                                                    condition = "input.legend_align == 'bottom'",
                                                                                                    numericInput("legend_adjustment", "Függőleges kiigazítás:", 
                                                                                                                 value = 0.3, min = 0, max = 1, step = 0.05)
                                                                                                  ) 
                                                                                           )
                                                                                         )
                                                                                ),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="X tengely", status="warning", solidHeader = T,
                                                                                         fluidRow(
                                                                                           column( style='border-right: 1px solid grey',
                                                                                                   width=6,
                                                                                                   h4("Tengelyfelirat"),
                                                                                                   textInput('x_text',
                                                                                                             'Felirat szövege:',
                                                                                                             value = "Default x tengely"),
                                                                                                   checkboxGroupButtons(
                                                                                                     inputId = "x_font",
                                                                                                     label = "Kiemelése:",
                                                                                                     choiceNames = list( icon("bold"), icon("italic")),
                                                                                                     choiceValues = list("bold", "italic"),
                                                                                                     justified = TRUE,
                                                                                                     status = "primary"
                                                                                                   ),
                                                                                                   numericInput("x_size", "Betűmérete:", "12"),
                                                                                                   colourpicker::colourInput("x_color", "Színe", value = "black")
                                                                                           ),
                                                                                           column(
                                                                                             width=6,
                                                                                             h4("Tengely/skála"),
                                                                                             sliderInput("xscale_angle", label = "Elforgatás", min = -180, max = 180, value = 0, step = 15, ticks = F, round = T, post = "°"),
                                                                                             numericInput("xscale_size", "Betűmérete:", "12"),
                                                                                             colourpicker::colourInput("xscale_fontcolor", "Színe", value = "black"),
                                                                                             br(),
                                                                                             numericInput("xscale_width", "Tengelyvonal vastagsága:", "1"),
                                                                                             colourpicker::colourInput("xscale_color", "Tengelyvonal színe", value = "dimgray"),
                                                                                             checkboxInput("xscale_tick", "Osztásközök mutatása", value = T, width = NULL)
                                                                                           )
                                                                                         )
                                                                                ),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Y tengely", status="warning", solidHeader = T,
                                                                                         fluidRow(
                                                                                           column( style='border-right: 1px solid grey',
                                                                                                   width=6,
                                                                                                   h4("Tengelyfelirat"),
                                                                                                   textInput('y_text',
                                                                                                             'Felirat szövege:',
                                                                                                             value = "Default y tengely"),
                                                                                                   checkboxGroupButtons(
                                                                                                     inputId = "y_font",
                                                                                                     label = "Kiemelése:",
                                                                                                     choiceNames = list( icon("bold"), icon("italic")),
                                                                                                     choiceValues = list("bold", "italic"),
                                                                                                     justified = TRUE,
                                                                                                     status = "primary"
                                                                                                   ),
                                                                                                   numericInput("y_size", "Betűmérete:", "12"),
                                                                                                   colourpicker::colourInput("y_color", "Színe", value = "black")
                                                                                           ),
                                                                                           column(
                                                                                             width=6,
                                                                                             h4("Tengely/skála"),
                                                                                             sliderInput("yscale_angle", label = "Elforgatás", min = -180, max = 180, value = 0, step = 15, ticks = F, round = T, post = "°"),
                                                                                             numericInput("yscale_size", "Betűmérete:", "12"),
                                                                                             colourpicker::colourInput("yscale_fontcolor", "Színe", value = "black"),
                                                                                             br(),
                                                                                             numericInput("yscale_width", "Tengelyvonal vastagsága:", "1"),
                                                                                             colourpicker::colourInput("yscale_color", "Tengelyvonal színe", value = "dimgray"),
                                                                                             checkboxInput("yscale_tick", "Osztásközök mutatása", value = T, width = NULL)
                                                                                           )
                                                                                         )
                                                                                ),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Adatfeliratok", status="warning", solidHeader = T,
                                                                                         fluidRow(
                                                                                           column(
                                                                                             width=6,
                                                                                             checkboxGroupButtons(
                                                                                               inputId = "label_font",
                                                                                               label = "Kiemelése:",
                                                                                               choiceNames = list( icon("bold"), icon("italic")),
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
                                                                        
                                                                        
                                                                        tabItem(tabName = "szinezes", h2("Színek"),
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Alapvető színek", status="warning", solidHeader = T,
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
                                                                                
                                                                                boxPlus( width=12, collapsed=T, collapsible=T, closable=F, title="Feltételes formázás", status="warning", solidHeader = T,
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
                                                               
                                                               column(8,
                                                                      
                                                                      #plotly::plotlyOutput('Alap_arfolyama_colplot'),
                                                                      
                                                                      boxPlus( width = 12, status = "danger",
                                                                               
                                                                               tabsetPanel(
                                                                                 id = "chart_tabset",
                                                                                 #line chart
                                                                                 tabPanel('Táblázat', DT::DTOutput("tabletab")),
                                                                                 tabPanel('Halmozott oszlop', plotly::plotlyOutput('barplot')),
                                                                                 tabPanel('Csoportosított oszlop', plotly::plotlyOutput('barplotclust')),
                                                                                 tabPanel('Halmozott sávdiagram', plotly::plotlyOutput('savplot')),
                                                                                 tabPanel('Csoportosított sávdiagram', plotly::plotlyOutput('savplotclust')),
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
                                                                      screenshotButton(id="barplot", timer=1)
                                                               )
                                                               
                                                               
                                                             )
                                                             
                                                             
                                       ),
                                       
                                       
                                       rightsidebar =
                                         rightSidebar(
                                           background = "dark",
                                           #width = 300,
                                           rightSidebarTabContent( id = 1, title = "Dátum", active = TRUE, icon = "calendar",
                                                                   
                                                                   
                                                                   
                                           ),
                                           
                                           rightSidebarTabContent( id = 2, title = "Érték és kategória",
                                                                   
                                                                   
                                           ),
                                           rightSidebarTabContent( id = 3, title = "Szűrők",
                                                                   
                                                                   
                                           )
                                         ),
                                       
                                       title = "Formázás",
                                       skin = "yellow"
  )
    
    
    
  }



shinyApp(ui = ui, server = server)



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