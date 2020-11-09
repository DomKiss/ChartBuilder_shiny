library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

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
            icon = icon("sliders"),
            textInput('title_text',
                      'Cím megadása:',
                      value = "Cím"),
            prettySwitch(
              inputId = "title_switch",
              label = "Részletes beállítások",
              fill = TRUE,
              status = "primary"
            ),
            conditionalPanel(
              condition = "input.title_switch > 0",
              numericInput("title_size", "Cím mérete:", "22"),
              colourpicker::colourInput("title_color", "Cím színe", value = "black")
            )
          )
        )
    ),
    
    sidebar = dashboardSidebar( ),
    
    body = dashboardBody(
      #setShadow(class = "dropdown-menu"),
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
        )
      ),
    
    title = "Formázás",
    skin = "yellow"
  )

shinyApp(ui = ui, server = server)