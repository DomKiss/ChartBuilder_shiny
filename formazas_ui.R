library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)


ui <-
  dashboardPagePlus(
    header = dashboardHeaderPlus(
      fixed = FALSE,
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears",
      left_menu =
        tagList(
          dropdownBlock(
            id = "dropdown_title",
            title = "Cím",
            icon = icon("sliders")
          )
        )
    ),
    
    sidebar = dashboardSidebar( ),
    
    body = dashboardBody(
      setShadow(class = "dropdown-menu"),
      br(),
      br(),
      tabsetPanel(
        id = "chart_tabset", 
        selected = NULL, 
        type ="pills", #"tabs",
        tabPanel(
          'Bar chart', 
          plotly::plotlyOutput('Alap_arfolyama_colplot')
        ),
        tabPanel(
          'Temp chart', 
          "empty"
        )
      
      )

    ),
    
  
    rightsidebar =
      rightSidebar(
        background = "dark",
        width = 300,
        rightSidebarTabContent(
          id = 1,
          title = "Általános",
          icon = "cog",
          active = TRUE,
          fluidRow(column(
            width = 12,
            colourpicker::colourInput("col_base", "Háttér szín", value = "white"),
            selectInput(
              "font_type",
              "Betűtípus",
              selected = 'F',
              choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
            )
          ))
        ),
        rightSidebarTabContent(
          id = 2,
          title = "Színpaletta",
          icon = "paint-brush",
          fluidRow(column(6, h4("Alapvető színek")), column(6, h4(
            "Feltételes formázás színei színei"
          ))),
          fluidRow(
            column(
              width = 6,
              colourpicker::colourInput("col_1", "Szín #1", value = "white"),
              colourpicker::colourInput("col_2", "Szín #2", value = "white"),
              colourpicker::colourInput("col_3", "Szín #3", value = "white"),
              colourpicker::colourInput("col_4", "Szín #4", value = "white"),
              colourpicker::colourInput("col_5", "Szín #5", value = "white"),
              colourpicker::colourInput("col_6", "Szín #6", value = "white"),
              colourpicker::colourInput("col_7", "Szín #7", value = "white"),
              colourpicker::colourInput("col_8", "Szín #8", value = "white")
            ),
            column(
              6,
              colourpicker::colourInput("col_felt_1", "Szín +", value = "green"),
              colourpicker::colourInput("col_felt_2", "Szín -", value = "red")
            )
          )
        ),
        rightSidebarTabContent(
          id = 3,
          title = "Szövegbeállítások",
          icon = "font",
          radioGroupButtons(
            inputId = "text",
            choices=c("Cím", "X tengely", "Y tengely", "Adatfelirat"),
            justified = TRUE,
            direction = "horizontal",
            size = "sm",
            status = "primary"
          ),
          br(), ####kéne valami vonal
          conditionalPanel(
            condition = "input.text == 'Cím'",
            textInput('title_text',
                      'Cím megadása:',
                      value = "Default cím"),
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
            ),
            numericInput("title_size", "Cím betűmérete:", "22"),
            colourpicker::colourInput("title_color", "Cím színe:", value = "black")
          ),
          conditionalPanel(
            condition = "input.text == 'X tengely'",
            textInput('x_text',
                      'Tengelyfelirat (x) megadása:',
                      value = "Default x tengely"),
            checkboxGroupButtons(
              inputId = "x_font",
              label = "Tengelyfelirat (x) kiemelése:",
              choiceNames = list( icon("bold"), icon("italic")),
              choiceValues = list("bold", "italic"),
              justified = TRUE,
              status = "primary"
            ),
            numericInput("x_size", "Tengelyfelirat (x) betűmérete:", "12"),
            colourpicker::colourInput("x_color", "Tengelyfelirat (x) színe", value = "black")
          ),
          conditionalPanel(
            condition = "input.text == 'Y tengely'",
            textInput('y_text',
                      'Tengelyfelirat (y) megadása:',
                      value = "Default y tengely"),
            checkboxGroupButtons(
              inputId = "y_font",
              label = "Tengelyfelirat (x) kiemelése:",
              choiceNames = list( icon("bold"), icon("italic")),
              choiceValues = list("bold", "italic"),
              justified = TRUE,
              status = "primary"
            ),
            numericInput("y_size", "Tengelyfelirat (y) betűmérete:", "12"),
            colourpicker::colourInput("y_color", "Tengelyfelirat (y) színe", value = "black")
          )
          
        )
      ),
    
    title = "Formázás",
    skin = "yellow"
  )

shinyApp(ui = ui, server = server)

getwd()

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

