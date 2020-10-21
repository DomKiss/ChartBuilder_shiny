library(shiny)
library(dplyr)
library(ggplot2)
#install.packages("shinythemes")
#install.packages("plotly")
#extrafont::font_import()

#import_data
alapok_df<- read.csv("C:/Users/Domonkos/Desktop/bamosz/data_shiny.csv", encoding="UTF-8", stringsAsFactors=FALSE)
names(alapok_df)[1] <- "Datum"
alapok_df$Datum <- alapok_df$Datum %>% as.Date
alapok_df$EFF_HOZAM_1_EVES = readr::parse_number(alapok_df$EFF_HOZAM_1_EVES)/100
alapok_df$EFF_HOZAM_3_EVES = readr::parse_number(alapok_df$EFF_HOZAM_3_EVES)/100


#ui ######################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      #felteteles panelre kattintva nyissa csak meg a formazas inputokat
      selectizeInput(
        'AlapNeve',
        'Alap neve',
        choices = alapok_df$ALAP_NEVE,
        selected = alapok_df$ALAP_NEVE[1],
        multiple = TRUE
      ),
      shinythemes::themeSelector(),
      selectInput(
        "valueselect",
        "Érték kiválasztása",
        selected = 'ARFOLYAM',
        choices = c("ARFOLYAM", "EFF_HOZAM_1_EVES", "EFF_HOZAM_3_EVES")
      ),
      
      selectInput(
        "categoryselect",
        "Kategória kiválasztása",
        selected = 'Datum',
        choices = c(
          "Datum",
          "ALAP_NEVE",
          "ALAPKEZELO",
          "EFF_HOZAM_3_EVES",
          "BEFPOL_SZERINTI_KATEGORIA",
          "ISIN_KOD"
        )
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
          actionButton(inputId = "generalButton", label = "Általános"),
          conditionalPanel(
            condition = ("input.generalButton%2!=0"),
            colourpicker::colourInput("col", "Háttérszín", value = "grey"),
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
            "Betûtípus",
            selected = 'F',
            choices = c("Times", "Calibri", "Helvetica", "Open-Sans")
          ))
        ),
        column(
          2,
          actionButton(inputId = "yButton", label = "Y tengely"),conditionalPanel(condition = ("input.yButton%2!=0"),
                                                                                  selectInput(
                                                                                    "betutipus",
                                                                                    "Betûtípus",
                                                                                    selected = 'F',
                                                                                    choices = c("Times", "Calibri", "Helvetica")
                                                                                  ))
        ),
        column(
          2,
          actionButton(inputId = "jelmagyButton", label = "Jelmagyarázat"),conditionalPanel(condition = ("input.jelmagyButton%2!=0"),
                                                                                  selectInput(
                                                                                    "betutipus",
                                                                                    "Betûtípus",
                                                                                    selected = 'F',
                                                                                    choices = c("Times", "Calibri", "Helvetica")
                                                                                  ))
        ),
        column(
          2,
          actionButton(inputId = "adatfelButton", label = "Adatfeliratok és színek"),conditionalPanel(condition = ("input.adatfelButton%2!=0"),
                                                                                            selectInput(
                                                                                              "betutipus",
                                                                                              "Betûtípus",
                                                                                              selected = 'F',
                                                                                              choices = c("Times", "Calibri", "Helvetica")
                                                                                            ))
        ),
        
        
      )
    )))


# server ################################
server <- function(input, output, session){
 #default cím megadása
  observe({
    type <- input$categoryselect
    default_title <-
      ifelse(input$categoryselect=="Datum",
      paste(input$valueselect,
            "változása"), paste(input$valueselect,
                                        "megoszlása", input$categoryselect, "szerint"))
    updateTextInput(session, "AbraCime", value = default_title)
    output$plot <- renderPlot({
      plot(1:10, main = default_title, type = input$type)
    })
  })
  
  
  
 # updateSelectizeInput(session, 'AlapNeve', choices = alapok_df$ALAP_NEVE, server = TRUE)
  #fuggveny az arfolyam plothoz
  plot_alapok_arfolyama <-
    function(chart_tipus = geom_line(),
             kivalasztott_ertek = input$valueselect, kategoria=input$categoryselect) {
      alapok_df %>%
        filter(ALAP_NEVE %in% input$AlapNeve) %>%
        #azert kell get, mert igy adja vissza azt az oszlopot, amit nevben megadtunk
        ggplot(aes(x = get(kategoria), y = get(kivalasztott_ertek), fill=ALAP_NEVE)) +
        chart_tipus + ggtitle(input$AbraCime) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = input$col), plot.title = element_text(size =
                                                                                  input$Cimsize, family = input$betutipus))
    }
  #arfolyam output
  output$Alap_arfolyama_plot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_line())
  })
  #tablazat output
  output$Alap_arfolyama_table <- DT::renderDT({
    alapok_df %>% 
      filter(ALAP_NEVE == input$AlapNeve)
  })
  #scatter output
  output$Alap_arfolyama_Scatterplot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_point())
  })
  
  #barchart output
  output$Alap_arfolyama_colplot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_col())
  })
  #area output
  output$Alap_arfolyama_areaplot <- plotly::renderPlotly({
    plot_alapok_arfolyama(geom_area())
  })
  
  
}
shinyApp(ui = ui, server = server)

